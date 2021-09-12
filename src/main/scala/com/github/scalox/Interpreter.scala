package com.github.scalox

import scala.collection.mutable

class Interpreter {

  val globalEnv: Environment = {
    val e = new Environment()
    defineClockFunc(e)
    e
  }

  private val locals: mutable.Map[Expr, Int] = mutable.Map[Expr, Int]()

  def interpret(statements: Seq[Stmt]): Unit = {
    try {
      implicit val env: Environment = globalEnv
      statements.foreach(execute)
    } catch {
      case e: RuntimeError =>
        ErrorHandler.runtimeError(e)
    }
  }

  def execute(statement: Stmt)
             (implicit env: Environment): Unit = statement match {
    case PrintStmt(expr) => println(stringify(evaluate(expr)))
    case ExpressionStmt(expr) => evaluate(expr)
    case VarStmt(name, initializer) =>
      val value: Option[Any] = initializer.flatMap(evaluate)
      env.define(name, value)
    case BlockStmt(statements) =>
      val blockEnv: Environment = new Environment(Some(env))
      statements.foreach(execute(_)(blockEnv))
    case IfStmt(condition, thenBranch, elseBranch) =>
      val conditionValue: Boolean = isTruthy(evaluate(condition))
      if (conditionValue) {
        execute(thenBranch)
      } else {
        elseBranch.foreach(execute(_))
      }
    case WhileStmt(condition, body) =>
      try {
        while (isTruthy(evaluate(condition))) {
          execute(body)
        }
      } catch {
        case _: BreakException =>
      }

    case BreakStmt => throw BreakException()
    case stmt: FunctionStmt =>
      val func: LoxFunction = new LoxFunction(Some(stmt.name.lexeme), stmt.function, env)
      env.define(stmt.name, Some(func))
    case ReturnStmt(token, value) =>
      val returnValue: Option[Any] = value.flatMap(evaluate(_))
      throw ReturnException(returnValue)
    case ClassStmt(name, superclass, methodStmts) =>
      executeClassStatement(name, superclass, methodStmts)
  }

  private def executeClassStatement(name: Token,
                                    superclass: Option[VariableExpr],
                                    methodStmts: Seq[FunctionStmt])
                                   (implicit env: Environment): Unit = {
    val superclassValue: Option[LoxClass] = superclass match {
      case None => None
      case Some(e) => evaluate(e) match {
        case Some(superClassValue: LoxClass) => Some(superClassValue)
        case _ => throw RuntimeError(e.name, "Superclass must be a class.")
      }
    }
    env.define(name.lexeme, None)

    val envForMethodDeclaration: Environment = superclass match {
      case Some(e) =>
        val superEnv: Environment = new Environment(Some(env))
        superEnv.define("super", superclassValue)
        superEnv
      case None => env
    }

    val methods: Map[String, LoxFunction] = methodStmts.map(method => {
      val methodName: String = method.name.lexeme
      methodName -> new LoxFunction(Some(methodName), method.function, envForMethodDeclaration, isInitializer = methodName == "init")
    }).toMap
    val klass: LoxClass = new LoxClass(name.lexeme, superclassValue, methods)
    env.assign(name, Some(klass))
  }

  def interpret(expression: Expr): Unit = {
    try {
      implicit val env: Environment = globalEnv
      println(stringify(evaluate(expression)))
    } catch {
      case e: RuntimeError =>
        ErrorHandler.runtimeError(e)
    }
  }

  private def evaluate(expression: Expr)
                      (implicit env: Environment): Option[Any] = expression match {
    case BinaryExpr(left, operator, right) =>
      evaluateBinaryExpression(left, operator, right)
    case GroupingExpr(expression) => evaluate(expression)
    case LiteralExpr(value) => value
    case UnaryExpr(operator, right) =>
      evaluateUnaryExpression(operator, right)
    case ConditionalExpr(condition, thenBranch, elseBranch) =>
      val conditionValue: Option[Any] = evaluate(condition)
      if (isTruthy(conditionValue)) {
        evaluate(thenBranch)
      } else {
        evaluate(elseBranch)
      }
    case VariableExpr(name) =>
      lookUpVariable(name, expression)

    case AssignExpr(name, value) =>
      evaluateAssignExpression(expression, name, value)

    case LogicalExpr(left, operator, right) =>
      evaluateLogicalExpression(left, operator, right)

    case CallExpr(callee, paren, args) =>
      evaluateCallExpression(callee, paren, args)

    case expr: FunctionExpr => Some(new LoxFunction(name = None,
      declaration = expr,
      closure = env))

    case GetExpr(obj, name) => evaluate(obj) match {
      case Some(inst: LoxInstance) => inst.get(name)
      case _ => throw RuntimeError(name, "Only instances have properties.")
    }
    case SetExpr(obj, name, value) => evaluateSetExpression(obj, name, value)
    case ThisExpr(keyword) => lookUpVariable(keyword, expression)
    case SuperExpr(_, methodToken) => evaluateSuperExpression(expression, methodToken)
  }

  private def evaluateBinaryExpression(left: Expr, operator: Token, right: Expr)
                                      (implicit env: Environment): Option[Any] = {
    val leftValue: Option[Any] = evaluate(left)
    val rightValue: Option[Any] = evaluate(right)
    operator.tokenType match {
      case TokenType.MINUS =>
        val (left, right) = getOperandsAsNumbers(operator, leftValue, rightValue)
        Some(left - right)
      case TokenType.SLASH =>
        val (left, right) = getOperandsAsNumbers(operator, leftValue, rightValue)
        if (right == 0D) {
          throw RuntimeError(operator, "/ by zero")
        } else {
          Some(left / right)
        }
      case TokenType.STAR =>
        val (left, right) = getOperandsAsNumbers(operator, leftValue, rightValue)
        Some(left * right)
      case TokenType.PLUS =>
        (leftValue, rightValue) match {
          case (Some(d1: Double), Some(d2: Double)) => Some(d1 + d2)
          case (Some(s1: String), Some(s2: String)) => Some(s1 + s2)
          case (Some(s1: String), Some(x)) => Some(s1 + x.toString)
          case (Some(x), Some(s1: String)) => Some(x.toString + s1)
          case _ => throw RuntimeError(operator, "Operands must be two numbers or two strings.")
        }
      case TokenType.LESS =>
        (leftValue, rightValue) match {
          case (Some(d1: Double), Some(d2: Double)) => Some(d1 < d2)
          case (Some(s1: String), Some(s2: String)) => Some(s1 < s2)
          case _ => throw RuntimeError(operator, "Operands must be two numbers or two strings.")
        }
      case TokenType.LESS_EQUAL =>
        (leftValue, rightValue) match {
          case (Some(d1: Double), Some(d2: Double)) => Some(d1 <= d2)
          case (Some(s1: String), Some(s2: String)) => Some(s1 <= s2)
          case _ => throw RuntimeError(operator, "Operands must be two numbers or two strings.")
        }
      case TokenType.GREATER =>
        (leftValue, rightValue) match {
          case (Some(d1: Double), Some(d2: Double)) => Some(d1 > d2)
          case (Some(s1: String), Some(s2: String)) => Some(s1 > s2)
          case _ => throw RuntimeError(operator, "Operands must be two numbers or two strings.")
        }
      case TokenType.GREATER_EQUAL =>
        (leftValue, rightValue) match {
          case (Some(d1: Double), Some(d2: Double)) => Some(d1 >= d2)
          case (Some(s1: String), Some(s2: String)) => Some(s1 >= s2)
          case _ => throw RuntimeError(operator, "Operands must be two numbers or two strings.")
        }
      case TokenType.EQUAL_EQUAL => Some(leftValue == rightValue)
      case TokenType.BANG_EQUAL => Some(leftValue != rightValue)
      case _ => throw RuntimeError(operator, "Unrecognized binary operator.")
    }
  }

  private def evaluateUnaryExpression(operator: Token, right: Expr)
                                     (implicit env: Environment): Option[Any] = {
    val rightValue: Option[Any] = evaluate(right)
    operator.tokenType match {
      case TokenType.MINUS => Some(-getOperandAsNumber(operator, rightValue))
      case TokenType.BANG => Some(!isTruthy(rightValue))
      case _ => throw RuntimeError(operator, "Unrecognized unary operator.")
    }
  }

  private def evaluateAssignExpression(expression: Expr, name: Token, value: Expr)
                                      (implicit env: Environment): Option[Any] = {
    val evaluatedValue = evaluate(value)
    val scopeDistanceOpt: Option[Int] = locals.get(expression)
    scopeDistanceOpt match {
      case Some(scopeDistance) =>
        env.assignAt(name, scopeDistance, evaluatedValue)
      case None =>
        globalEnv.assign(name, evaluatedValue)
    }
    evaluatedValue
  }

  private def evaluateLogicalExpression(left: Expr, operator: Token, right: Expr)
                                       (implicit env: Environment): Option[Any] = {
    val leftValue: Option[Any] = evaluate(left)
    operator.tokenType match {
      //short circuit
      case TokenType.OR => if (isTruthy(leftValue)) {
        leftValue
      } else {
        evaluate(right)
      }
      case TokenType.AND => if (!isTruthy(leftValue)) {
        leftValue
      } else {
        evaluate(right)
      }
      case _ => throw RuntimeError(operator, "Unrecognized logical operator.")
    }
  }

  private def evaluateCallExpression(callee: Expr, callToken: Token, args: Seq[Expr])(implicit env: Environment): Option[Any] = {
    val calleeValue: Option[Any] = evaluate(callee)
    val argsValues: Seq[Option[Any]] = args.map(evaluate(_))

    calleeValue match {
      case Some(callable: LoxCallable) =>
        implicit val implInterpreter: Interpreter = this
        if (argsValues.size != callable.arity()) {
          throw RuntimeError(callToken, s"Expected ${callable.arity()} arguments but got ${argsValues.size}.")
        }
        callable.call(argsValues)
      case _ => throw RuntimeError(callToken, "Can only call functions and classes.")
    }
  }

  private def evaluateSetExpression(obj: Expr, name: Token, value: Expr)
                                   (implicit env: Environment): Option[Any] = {
    evaluate(obj) match {
      case Some(inst: LoxInstance) =>
        val evaluatedValue: Option[Any] = evaluate(value)
        inst.set(name, evaluatedValue)
        evaluatedValue
      case _ => throw RuntimeError(name, "Only instances have fields.")
    }
  }

  private def evaluateSuperExpression(expression: Expr, methodToken: Token)
                                     (implicit env: Environment): Option[Any] = {
    try {
      val distance: Int = locals(expression)
      val superclass: LoxClass = (env.getAt("super", distance) match {
        case Some(x: LoxClass) => Some(x)
        case _ => None
      }).get
      val obj: LoxInstance = (env.getAt("this", distance - 1) match {
        case Some(x: LoxInstance) => Some(x)
        case _ => None
      }).get
      val method: LoxFunction = superclass.findMethod(methodToken.lexeme)
        .getOrElse(throw RuntimeError(methodToken, s"Undefined property '${methodToken.lexeme}'."))
      Some(method.bind(obj))
    } catch {
      case e: RuntimeError => throw e
      case _: Throwable => throw new Exception("Invalid compiler state.")
    }
  }

  def resolve(expr: Expr, depth: Int): Unit = {
    locals.put(expr, depth)
  }

  private def lookUpVariable(name: Token, expression: Expr)
                            (implicit env: Environment): Option[Any] = {
    val scopeDistanceOpt: Option[Int] = locals.get(expression)
    scopeDistanceOpt match {
      case Some(scopeDistance) => env.getAt(name, scopeDistance) match {
        case Some(v) => Some(v)
        case None => throw RuntimeError(name, "Uninitialized variable.")
      }
      case None => globalEnv.get(name)
    }
  }

  private def isTruthy(exprValue: Option[Any]): Boolean = exprValue match {
    case None | Some(false) => false
    case _ => true
  }

  private def getOperandAsNumber(operator: Token, operand: Option[Any]): Double = {
    operand match {
      case Some(d: Double) => d
      case _ => throw RuntimeError(operator, "Operand must be a number.")
    }
  }

  private def getOperandsAsNumbers(operator: Token, operand1: Option[Any], operand2: Option[Any]): (Double, Double) = {
    (operand1, operand2) match {
      case (Some(d1: Double), Some(d2: Double)) => (d1, d2)
      case _ => throw RuntimeError(operator, "Operands must be numbers.")
    }
  }

  def stringify(evaluationRes: Option[Any]): String = evaluationRes match {
    case None => "nil"
    case Some(d: Double) =>
      val numberText: String = d.toString
      if (numberText.endsWith(".0")) {
        numberText.substring(0, numberText.length - 2)
      } else {
        numberText
      }
    case Some(x) => x.toString
  }

  private def defineClockFunc(env: Environment): Unit = {
    env.define("clock", Some(new LoxCallable {
      override def arity(): Int = 0

      override def call(arguments: Seq[Option[Any]])
                       (implicit interpreter: Interpreter): Option[Any] = {
        Some(System.currentTimeMillis() / 1E3)
      }

      override def toString: String = "<native fn>"
    }))
  }
}

case class RuntimeError(token: Token, message: String) extends RuntimeException(message)

case class BreakException() extends Exception(null, null, false, false)

case class ReturnException(value: Option[Any]) extends Exception(null, null, false, false)
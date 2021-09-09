package com.github.scalox

class Interpreter {
  private val globalEnvironment: Environment = new Environment()

  def interpret(statements: Seq[Stmt]): Unit = {
    try {
      implicit val env: Environment = globalEnvironment
      statements.foreach(execute)
    } catch {
      case e: RuntimeError =>
        ErrorHandler.runtimeError(e)
    }
  }

  private def execute(statement: Stmt)
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
  }

  def interpret(expression: Expr): Unit = {
    try {
      implicit val env: Environment = globalEnvironment
      println(stringify(evaluate(expression)))
    } catch {
      case e: RuntimeError =>
        ErrorHandler.runtimeError(e)
    }
  }

  private def evaluate(expression: Expr)
                      (implicit env: Environment): Option[Any] = expression match {
    case BinaryExpr(left, operator, right) =>
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
    case GroupingExpr(expression) => evaluate(expression)
    case LiteralExpr(value) => value
    case UnaryExpr(operator, right) =>
      val rightValue: Option[Any] = evaluate(right)
      operator.tokenType match {
        case TokenType.MINUS => Some(-getOperandAsNumber(operator, rightValue))
        case TokenType.BANG => Some(!isTruthy(rightValue))
        case _ => throw RuntimeError(operator, "Unrecognized unary operator.")
      }
    case TernaryExpr(left, leftOperator, middle, rightOperator, right) =>
      (leftOperator.tokenType, rightOperator.tokenType) match {
        case (TokenType.QUESTION_MARK, TokenType.COLON) =>
          val leftValue: Option[Any] = evaluate(left)
          if (isTruthy(leftValue)) {
            evaluate(middle)
          } else {
            evaluate(right)
          }
        case _ => throw RuntimeError(leftOperator, "Unrecognized ternary operator.")
      }
    case VariableExpr(name) => env.get(name) match {
      case Some(v) => Some(v)
      case None => throw RuntimeError(name, "Uninitialized variable.")
    }
    case AssignExpr(name, value) =>
      val evaluatedValue = evaluate(value)
      env.assign(name, evaluatedValue)
      evaluatedValue
    case LogicalExpr(left, operator, right) =>
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
}

case class RuntimeError(token: Token, message: String) extends RuntimeException(message)

case class BreakException() extends Exception
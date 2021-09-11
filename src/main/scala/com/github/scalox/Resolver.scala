package com.github.scalox

import scala.collection.mutable

class Resolver(interpreter: Interpreter) {

  private val scopes: mutable.Stack[mutable.Map[String, Boolean]] = mutable.Stack()
  private var currentFunction: FunctionType.Value = FunctionType.NONE
  private var currentClass: ClassType.Value = ClassType.NONE

  def resolve(statements: Seq[Stmt]): Unit = {
    statements.foreach(resolve)
  }

  def resolve(statement: Stmt): Unit = statement match {
    case BlockStmt(statements) =>
      beginScope()
      resolve(statements)
      endScope()
    case VarStmt(name, initializer) =>
      declare(name)
      initializer.foreach(resolve)
      define(name)
    case funcStmt@FunctionStmt(name, _) =>
      declare(name)
      define(name)
      resolveFunction(funcStmt, FunctionType.FUNCTION)
    case ExpressionStmt(expr) =>
      resolve(expr)
    case IfStmt(condition, thenBranch, elseBranch) =>
      resolve(condition)
      resolve(thenBranch)
      elseBranch.foreach(resolve)
    case PrintStmt(expr) =>
      resolve(expr)
    case ReturnStmt(token, value) =>
      currentFunction match {
        case FunctionType.NONE => ErrorHandler.error(token, "Can't return from top-level code.")
        case FunctionType.INITIALIZER if value.nonEmpty => ErrorHandler.error(token, "Can't return a value from an initializer")
        case _ =>
      }
      value.foreach(resolve)
    case WhileStmt(condition, body) =>
      resolve(condition)
      resolve(body)
    case BreakStmt =>
    case ClassStmt(name, methods) =>
      val enclosingClass: ClassType.Value = currentClass
      currentClass = ClassType.CLASS
      declare(name)
      define(name)

      beginScope()
      scopes.head.put("this", true)

      methods.foreach(method => {
        val declaration: FunctionType.Value = if(method.name.lexeme == "init"){
          FunctionType.INITIALIZER
        }else{
          FunctionType.METHOD
        }
        resolveFunction(method, declaration)
      })

      endScope()
      currentClass = enclosingClass
  }

  def resolve(expression: Expr): Unit = expression match {
    case VariableExpr(name) =>
      if (scopes.headOption.flatMap(_.get(name.lexeme)).contains(false)) {
        ErrorHandler.error(name, "Can't read local variable in its own initializer.")
      }
      resolveLocal(expression, name)
    case AssignExpr(name, value) =>
      resolve(value)
      resolveLocal(expression, name)
    case BinaryExpr(left, _, right) =>
      resolve(left)
      resolve(right)
    case GroupingExpr(expression) =>
      resolve(expression)
    case LiteralExpr(_) =>
    case UnaryExpr(_, right) =>
      resolve(right)
    case ConditionalExpr(condition, thenBranch, elseBranch) =>
      resolve(condition)
      resolve(thenBranch)
      resolve(elseBranch)
    case LogicalExpr(left, _, right) =>
      resolve(left)
      resolve(right)
    case CallExpr(callee, _, arguments) =>
      resolve(callee)
      arguments.foreach(resolve)
    case FunctionExpr(_, body) =>
      resolve(body)
    case GetExpr(obj, _) =>
      resolve(obj)
    case SetExpr(obj, _, value) =>
      resolve(value)
      resolve(obj)
    case ThisExpr(keyword) => currentClass match {
      case ClassType.NONE =>
        ErrorHandler.error(keyword, "Can't use 'this' outside of a class.")
      case ClassType.CLASS =>
        resolveLocal(expression, keyword)
    }
  }

  private def resolveLocal(expr: Expr, name: Token): Unit = {
    var resolved: Boolean = false
    scopes.zipWithIndex.foreach({
      case (scope, i) => {
        if(!resolved){
          if(scope.contains(name.lexeme)){
            interpreter.resolve(expr, i)
            resolved = true
          }
        }
      }
    })
  }

  private def resolveFunction(stmt: FunctionStmt,
                              functionType: FunctionType.Value): Unit = {
    val enclosingFunction: FunctionType.Value = currentFunction
    currentFunction = functionType
    beginScope()
    stmt.function.parameters.foreach(param => {
      declare(param)
      define(param)
    })
    resolve(stmt.function.body)
    endScope()
    currentFunction = enclosingFunction
  }

  private def declare(name: Token): Unit = {
    scopes.headOption.foreach(topScope => {
      if(topScope.contains(name.lexeme)){
        ErrorHandler.error(name, "Already a variable with this name in this scope.")
      }
      topScope.put(name.lexeme, false)
    })
  }

  private def define(name: Token): Unit = {
    scopes.headOption.foreach(topScope => {
      topScope.put(name.lexeme, true)
    })
  }

  private def beginScope(): Unit = {
    scopes.push(mutable.Map[String, Boolean]())
  }

  private def endScope(): Unit = {
    scopes.pop()
  }
}

object FunctionType extends Enumeration{
  val NONE, FUNCTION, METHOD, INITIALIZER = Value
}

object ClassType extends Enumeration {
  val NONE, CLASS = Value
}
package com.github.scalox

object ASTPrinter {

  def print(expr: Expr): String = expr match {
    case BinaryExpr(left, operator, right) =>
      parenthesize(operator.lexeme, left, right)
    case GroupingExpr(expression) =>
      parenthesize("group", expression)
    case LiteralExpr(value) =>
      value.map(_.toString).getOrElse("nil")
    case UnaryExpr(operator, right) =>
      parenthesize(operator.lexeme, right)
    case ConditionalExpr(condition, thenBranch, elseBranch) =>
     parenthesize("conditional", condition, thenBranch, elseBranch)
    case VariableExpr(name) =>
      s"(var ${name.lexeme})"
    case AssignExpr(name, value) => parenthesize(name.lexeme, value)
    case LogicalExpr(left, operator, right) => parenthesize(operator.lexeme, left, right)
    case CallExpr(callee, paren, arguments) => parenthesize(paren.lexeme, Seq(callee) ++ arguments:_*)
    case FunctionExpr(parameters, body) => s"(anonymous function(${parameters.size}))"
    case GetExpr(obj, name) => parenthesize(name.lexeme, obj)
    case SetExpr(obj, name, value) =>parenthesize(name.lexeme, obj, value)
    case ThisExpr(keyword) => keyword.lexeme
  }

  private def parenthesize(name: String, exprs: Expr*): String = {
    s"(${(Seq(name) ++ exprs.map(print)).mkString(" ")})"
  }
}

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
    case TernaryExpr(left, leftOperator, middle, rightOperator, right) =>
      s"(${leftOperator.lexeme} ${print(left)} (${rightOperator.lexeme} ${print(middle)} ${print(right)}))"
    case VariableExpr(name) =>
      s"(var ${name.lexeme})"
    case AssignExpr(name, value) => parenthesize(name.lexeme, value)
  }

  private def parenthesize(name: String, exprs: Expr*): String = {
    s"(${(Seq(name) ++ exprs.map(print)).mkString(" ")})"
  }
}

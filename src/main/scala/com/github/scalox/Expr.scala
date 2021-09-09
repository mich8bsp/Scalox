package com.github.scalox

sealed trait Expr

case class BinaryExpr(
                       left: Expr,
                       operator: Token,
                       right: Expr
                     ) extends Expr

case class GroupingExpr(expression: Expr) extends Expr

case class LiteralExpr(value: Option[Any]) extends Expr

case class UnaryExpr(operator: Token, right: Expr) extends Expr

case class ConditionalExpr(
                        condition: Expr,
                        thenBranch: Expr,
                        elseBranch: Expr
                      ) extends Expr

case class VariableExpr(name: Token) extends Expr

case class AssignExpr(name: Token, value: Expr) extends Expr

case class LogicalExpr(left: Expr, operator: Token, right: Expr) extends Expr
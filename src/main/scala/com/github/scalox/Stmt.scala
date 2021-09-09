package com.github.scalox

sealed trait Stmt

case class PrintStmt(expr: Expr) extends Stmt
case class ExpressionStmt(expr: Expr) extends Stmt
case class VarStmt(name: Token, initializer: Option[Expr]) extends Stmt
case class BlockStmt(statements: Seq[Stmt]) extends Stmt
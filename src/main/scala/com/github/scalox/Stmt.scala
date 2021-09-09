package com.github.scalox

sealed trait Stmt

case class PrintStmt(expr: Expr) extends Stmt

case class ExpressionStmt(expr: Expr) extends Stmt

case class VarStmt(name: Token, initializer: Option[Expr]) extends Stmt

case class BlockStmt(statements: Seq[Stmt]) extends Stmt

case class IfStmt(condition: Expr, thenBranch: Stmt, elseBranch: Option[Stmt]) extends Stmt

case class WhileStmt(condition: Expr, body: Stmt) extends Stmt
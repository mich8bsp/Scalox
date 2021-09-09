package com.github.scalox

import com.github.scalox.TokenType._

import scala.collection.mutable

class Parser(tokens: Seq[Token]) {
  private var current: Int = 0

  def parseStatements(): Seq[Stmt] = try {
    // program -> declaration* EOF
    val statements: mutable.Buffer[Stmt] = mutable.Buffer()
    while(!isAtEnd){
      declaration().foreach(statements.append)
    }
    statements.toSeq
  } finally {
    current = 0
  }

  def parseExpression(): Option[Expr] = try {
    Some(expression())
  } catch {
    case _: ParseError =>
      None
  } finally {
    current = 0
  }

  private def declaration(): Option[Stmt] = {
    // declaration -> varDeclaration | statement
    try {
      if(matchExpr(VAR)){
        Some(varDeclaration())
      }else{
        Some(statement())
      }
    }catch {
      case _: ParseError =>
        synchronize()
        None
    }
  }

  private def varDeclaration(): Stmt = {
    // varDeclaration -> "var" IDENTIFIER ( "=" expression )? ";"
    val name: Token = consumeAndGet(IDENTIFIER, "Expect variable name.")

    val initializer: Option[Expr] = if(matchExpr(EQUAL)){
      Some(expression())
    }else{
      None
    }

    consume(SEMICOLON, "Expect ';' after variable declaration.")
    VarStmt(name = name, initializer = initializer)
  }

  private def statement(): Stmt = {
    // statement -> expressionStatement | printStatement | blockStatement
    if(matchExpr(PRINT)){
      printStatement()
    }else if(matchExpr(LEFT_BRACE)){
      BlockStmt(blockStatement())
    }else{
      expressionStatement()
    }
  }

  private def printStatement(): Stmt = {
    val expr = expression()
    consume(SEMICOLON, "Expect ';' after value.")
    PrintStmt(expr)
  }

  private def expressionStatement(): Stmt = {
    val expr = expression()
    consume(SEMICOLON, "Expect ';' after value.")
    ExpressionStmt(expr)
  }

  private def blockStatement(): Seq[Stmt] = {
    // blockStatement -> "{" declaration* "}"
    val statements: mutable.Buffer[Stmt] = mutable.Buffer()
    while(!check(RIGHT_BRACE) && !isAtEnd){
      declaration().foreach(statements.append)
    }

    consume(RIGHT_BRACE, "Expect '}' after block.")
    statements.toSeq
  }

  private def expression(): Expr = {
    assignment()
  }

  private def assignment(): Expr = {
    val expr = ternary()

    if(matchExpr(EQUAL)){
      val equals = previous
      val value = assignment()

      expr match {
        case VariableExpr(name) => AssignExpr(name, value)
        case _ => error(equals, "Invalid assignment target.")
          expr
      }
    }else{
      expr
    }
  }

  private def ternary(): Expr = {
    //ternary -> equality ("?") expression (":") expression | equality
    var expr = equality()

    if (matchExpr(QUESTION_MARK)) {
      val leftOperator = previous
      val exprIfTrue = expression()
      if (matchExpr(COLON)) {
        val rightOperator = previous
        val exprIfFalse = expression()
        expr = TernaryExpr(
          left = expr,
          middle = exprIfTrue,
          right = exprIfFalse,
          leftOperator = leftOperator,
          rightOperator = rightOperator
        )
      } else {
        throw error(peek, "Expected : after expression.")
      }
    }

    expr
  }

  private def equality(): Expr = {
    // equality  → comparison ( ( "!=" | "==" ) comparison )* ;
    binaryExpr(comparison, BANG_EQUAL, EQUAL_EQUAL)
  }

  private def comparison(): Expr = {
    //comparison  → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
    binaryExpr(term, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)
  }

  private def term(): Expr = {
    //term → factor ( ( "-" | "+" ) factor )* ;
    binaryExpr(factor, MINUS, PLUS)
  }

  private def factor(): Expr = {
    //factor → unary ( ( "/" | "*" ) unary )* ;
    binaryExpr(unary, SLASH, STAR)
  }

  private def unary(): Expr = {
    //unary → ( "!" | "-" ) unary | primary ;
    if (matchExpr(BANG, MINUS)) {
      val operator: Token = previous
      val right: Expr = unary()
      UnaryExpr(operator = operator, right = right)
    } else {
      primary()
    }
  }

  private def primary(): Expr = {
    //primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER ;
    if (matchExpr(FALSE)) {
      LiteralExpr(Some(false))
    } else if (matchExpr(TRUE)) {
      LiteralExpr(Some(true))
    } else if (matchExpr(NIL)) {
      LiteralExpr(None)
    } else if (matchExpr(NUMBER, STRING)) {
      LiteralExpr(previous.literal)
    } else if (matchExpr(IDENTIFIER)){
      VariableExpr(previous)
    } else if (matchExpr(LEFT_PAREN)) {
      val expr = expression()
      consume(RIGHT_PAREN, "Expect ')' after expression.")
      GroupingExpr(expr)
    } else {
      throw error(peek, "Expect expression.")
    }
  }

  private def binaryExpr(subExpression: () => Expr,
                         tokenTypes: TokenType*): Expr = {
    val tokensValidOnlyAsBinary = tokenTypes.filterNot(_ == TokenType.MINUS)
    if(matchExpr(tokensValidOnlyAsBinary: _*)){
      //binary expression is missing left side expression
      subExpression() // read right side expression and discard
      throw error(previous, "Expect expression.")
    }else {
      var expr = subExpression()

      while (matchExpr(tokenTypes: _*)) {
        val operator: Token = previous
        val right: Expr = subExpression()
        expr = BinaryExpr(left = expr, operator = operator, right = right)
      }

      expr
    }
  }

  private def consume(tokenType: TokenType,
                      errorMessage: String): Unit = {
    if (check(tokenType)) {
      advance()
    } else {
      throw error(peek, errorMessage)
    }
  }

  private def consumeAndGet(tokenType: TokenType,
                            errorMessage: String): Token = {
    if(check(tokenType)){
      getAndAdvance()
    }else{
      throw error(peek, errorMessage)
    }
  }

  private def error(token: Token, errorMessage: String): ParseError = {
    ErrorHandler.error(token, errorMessage)
    new ParseError
  }

  private def matchExpr(expectedTypes: TokenType*): Boolean = {
    var matched: Boolean = false
    expectedTypes.foreach(expectedType => {
      if (!matched && check(expectedType)) {
        advance()
        matched = true
      }
    })

    matched
  }

  private def synchronize(): Unit = {
    advance()
    var syncSuccess = false
    while (!isAtEnd && !syncSuccess) {
      if (previous.tokenType == SEMICOLON) {
        syncSuccess = true
      } else {
        peek.tokenType match {
          case CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN =>
            syncSuccess = true
          case _ => advance()
        }
      }
    }
  }

  private def check(expectedType: TokenType): Boolean = {
    !isAtEnd && (peek.tokenType == expectedType)
  }

  private def advance(): Unit = {
    if (!isAtEnd) {
      current += 1
    }
  }

  private def getAndAdvance(): Token = {
    val t = peek
    advance()
    t
  }

  private def isAtEnd: Boolean = {
    peek.tokenType == TokenType.EOF
  }

  private def peek: Token = tokens(current)

  private def previous: Token = tokens(current - 1)

}

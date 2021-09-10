package com.github.scalox

import com.github.scalox.TokenType._

import scala.collection.mutable

class Parser(tokens: Seq[Token]) {
  private var current: Int = 0
  private var loopDepth: Int = 0

  def parseStatements(): Seq[Stmt] = try {
    // program -> declaration* EOF
    val statements: mutable.Buffer[Stmt] = mutable.Buffer()
    while (!isAtEnd) {
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
    // declaration -> funDeclaration | varDeclaration | statement
    try {
      if (check(FUN) && checkNext(IDENTIFIER)){
        consume(FUN, "")
        Some(funDeclaration("function"))
      }else if (matchExpr(VAR)) {
        Some(varDeclaration())
      } else {
        Some(statement())
      }
    } catch {
      case _: ParseError =>
        synchronize()
        None
    }
  }

  private def funDeclaration(kind: String): Stmt = {
    // funDeclaration -> "fun" function
    //function -> IDENTIFIER "(" parameters? ")" block
    //parameters -> IDENTIFIER ( "," IDENTIFIER )*
    val name: Token = consumeAndGet(IDENTIFIER, s"Expect $kind name.")

    FunctionStmt(name = name, function = functionBody(kind))
  }

  private def functionBody(kind: String): FunctionExpr = {
    consume(LEFT_PAREN, s"Expect '(' after $kind name.")
    val params: mutable.Buffer[Token] = mutable.Buffer[Token]()
    if(!check(RIGHT_PAREN)){
      do {
        if(params.size >= 255){
          error(peek, "Can't have more than 255 parameters.")
        }

        params.append(consumeAndGet(IDENTIFIER, "Expect parameter name."))
      } while (matchExpr(COMMA))
    }
    consume(RIGHT_PAREN, "Expect ')' after parameters.")
    consume(LEFT_BRACE, s"Expect '{' before $kind body.")
    val body: Seq[Stmt] = blockStatement() match {
      case BlockStmt(statements) => statements
      case _ => throw new Exception("Invalid parser state")
    }
    FunctionExpr(
      parameters = params.toSeq, body = body
    )
  }

  private def varDeclaration(): Stmt = {
    // varDeclaration -> "var" IDENTIFIER ( "=" expression )? ";"
    val name: Token = consumeAndGet(IDENTIFIER, "Expect variable name.")

    val initializer: Option[Expr] = if (matchExpr(EQUAL)) {
      Some(expression())
    } else {
      None
    }

    consume(SEMICOLON, "Expect ';' after variable declaration.")
    VarStmt(name = name, initializer = initializer)
  }

  private def statement(): Stmt = {
    // statement -> expressionStatement | forStatement | ifStatement | printStatement | whileStatement | blockStatement | breakStatement | returnStatement
    if (matchExpr(PRINT)) {
      printStatement()
    } else if (matchExpr(LEFT_BRACE)) {
      blockStatement()
    } else if (matchExpr(IF)) {
      ifStatement()
    } else if (matchExpr(WHILE)) {
      whileStatement()
    } else if (matchExpr(FOR)) {
      forStatement()
    } else if (matchExpr(BREAK)) {
      breakStatement()
    } else if(matchExpr(RETURN)){
      returnStatement()
    } else {
      expressionStatement()
    }
  }

  private def ifStatement(): Stmt = {
    // ifStatement -> "if" "(" expression ")" statement ( "else" statement )?
    consume(LEFT_PAREN, "Expect '(' after 'if'.")
    val condition: Expr = expression()
    consume(RIGHT_PAREN, "Expect ')' after if condition.")

    val thenBranch: Stmt = statement()
    val elseBranch: Option[Stmt] = if (matchExpr(ELSE)) {
      Some(statement())
    } else {
      None
    }
    IfStmt(condition, thenBranch, elseBranch)
  }

  private def whileStatement(): Stmt = {
    //whileStatement -> "while" "(" expression ")" statement
    consume(LEFT_PAREN, "Expect '(' after 'while'.")
    val condition: Expr = expression()
    consume(RIGHT_PAREN, "Expect ')' after while condition.")

    try {
      loopDepth += 1
      val loopStatement: Stmt = statement()
      WhileStmt(condition, loopStatement)
    } finally {
      loopDepth -= 1
    }

  }

  private def forStatement(): Stmt = {
    //forStatement -> "for" "(" (varDeclaration | exprStmt | ";") expression? ";" expression? ")" statement
    consume(LEFT_PAREN, "Expect '(' after 'for'.")

    val initializer: Option[Stmt] = if (matchExpr(SEMICOLON)) {
      None
    } else if (matchExpr(VAR)) {
      Some(varDeclaration())
    } else {
      Some(expressionStatement())
    }

    val condition: Expr = if (!check(SEMICOLON)) {
      expression()
    } else {
      LiteralExpr(Some(true))
    }
    consume(SEMICOLON, "Expect ';' after loop condition.")

    val increment: Option[Expr] = if (!check(SEMICOLON)) {
      Some(expression())
    } else {
      None
    }
    consume(RIGHT_PAREN, "Expect ')' after for clauses.")

    try {
      loopDepth += 1
      var body: Stmt = statement()
      increment.foreach(incr => {
        body = BlockStmt(Seq(body, ExpressionStmt(incr)))
      })

      body = WhileStmt(condition, body)

      initializer.foreach(init => {
        body = BlockStmt(Seq(init, body))
      })

      body
    } finally {
      loopDepth -= 1
    }
  }

  private def printStatement(): Stmt = {
    //printStatement -> "print" expression
    val expr = expression()
    consume(SEMICOLON, "Expect ';' after value.")
    PrintStmt(expr)
  }

  private def breakStatement(): Stmt = {
    if (loopDepth == 0) {
      error(previous, "Invalid 'break' outside of loop scope.")
    }
    consume(SEMICOLON, "Expect ';' after break.")
    BreakStmt
  }

  private def returnStatement(): Stmt = {
    // returnStatement -> "return" expression? ";"
    val token = previous
    val value: Option[Expr] = if(!check(SEMICOLON)){
      Some(expression())
    }else{
      None
    }

    consume(SEMICOLON, "Expect ';' after return value.")
    ReturnStmt(token = token, value = value)
  }

  private def expressionStatement(): Stmt = {
    // expressionStatement -> expression
    val expr = expression()
    consume(SEMICOLON, "Expect ';' after value.")
    ExpressionStmt(expr)
  }

  private def blockStatement(): Stmt = {
    // blockStatement -> "{" declaration* "}" ;
    val statements: mutable.Buffer[Stmt] = mutable.Buffer()
    while (!check(RIGHT_BRACE) && !isAtEnd) {
      declaration().foreach(statements.append)
    }

    consume(RIGHT_BRACE, "Expect '}' after block.")
    BlockStmt(statements.toSeq)
  }

  private def expression(): Expr = {
    // expression -> assignment
    assignment()
  }

  private def assignment(): Expr = {
    // assignment -> IDENTIFIER "=" assignment | ternary
    val expr = ternary()

    if (matchExpr(EQUAL)) {
      val equals = previous
      val value = assignment()

      expr match {
        case VariableExpr(name) => AssignExpr(name, value)
        case _ => error(equals, "Invalid assignment target.")
          expr
      }
    } else {
      expr
    }
  }

  private def ternary(): Expr = {
    //ternary -> logic_or ( "?" expression ":" ternary)?
    var expr = logicOr()

    if (matchExpr(QUESTION_MARK)) {
      val exprIfTrue = expression()
      consume(COLON, "Expect ';' after then branch of conditional expression.")
      val exprIfFalse = ternary()
      expr = ConditionalExpr(
        condition = expr,
        thenBranch = exprIfTrue,
        elseBranch = exprIfFalse
      )
    }

    expr
  }

  private def logicOr(): Expr = {
    // logic_or -> logic_and ( "or" logic_and )*
    var expr = logicAnd()

    while (matchExpr(OR)) {
      val operator = previous
      val right = logicAnd()
      expr = LogicalExpr(expr, operator, right)
    }

    expr
  }

  private def logicAnd(): Expr = {
    // logic_and -> equality ( "and" equality)*
    var expr = equality()

    while (matchExpr(AND)) {
      val operator = previous
      val right = equality()
      expr = LogicalExpr(expr, operator, right)
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
    //unary → ( "!" | "-" ) unary | call ;
    if (matchExpr(BANG, MINUS)) {
      val operator: Token = previous
      val right: Expr = unary()
      UnaryExpr(operator = operator, right = right)
    } else {
      call()
    }
  }

  private def call(): Expr = {
    // call -> primary ( "(" arguments? ")" )* ;
    val functionExpr: Expr = primary()
    val args: mutable.Buffer[Expr] = mutable.Buffer[Expr]()
    var parenToken: Option[Token] = None
    while (matchExpr(LEFT_PAREN)){
      if(!matchExpr(RIGHT_PAREN)){
        args.appendAll(arguments())
        parenToken = Some(consumeAndGet(RIGHT_PAREN, "Expect ')' after function arguments."))
      }else{
        parenToken = Some(previous)
      }
    }
    parenToken match {
      case Some(t) => CallExpr(callee = functionExpr, paren = t, arguments = args.toSeq)
      case None => functionExpr
    }
  }

  private def arguments(): Seq[Expr] = {
    // arguments -> expression ( "," expression )* ;
    val args = mutable.Buffer[Expr]()
    args.append(expression())
    while(matchExpr(COMMA)){
      if(args.size >= 255){
        error(peek, "Can't have more than 255 arguments.")
      }
      args.append(expression())
    }
    args.toSeq
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
    } else if (matchExpr(IDENTIFIER)) {
      VariableExpr(previous)
    } else if (matchExpr(LEFT_PAREN)) {
      val expr = expression()
      consume(RIGHT_PAREN, "Expect ')' after expression.")
      GroupingExpr(expr)
    } else if(matchExpr(FUN)){
      functionBody("function")
    } else {
      throw error(peek, "Expect expression.")
    }
  }

  private def binaryExpr(subExpression: () => Expr,
                         tokenTypes: TokenType*): Expr = {
    val tokensValidOnlyAsBinary = tokenTypes.filterNot(_ == TokenType.MINUS)
    if (matchExpr(tokensValidOnlyAsBinary: _*)) {
      //binary expression is missing left side expression
      subExpression() // read right side expression and discard
      throw error(previous, "Expect expression.")
    } else {
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
    if (check(tokenType)) {
      getAndAdvance()
    } else {
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

  private def checkNext(tokenType: TokenType): Boolean = {
    if(isAtEnd || tokens(current+1).tokenType == EOF) {
      false
    }else{
      tokens(current+1).tokenType == tokenType
    }
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

package com.github.scalox

import com.github.scalox.TokenType._

import scala.collection.mutable

class Scanner(source: String) {

  import Scanner._

  private var start: Int = 0
  private var current: Int = 0
  private var line: Int = 1

  def scanTokens(): Seq[Token] = {
    try {
      val tokens: mutable.Buffer[Token] = mutable.Buffer[Token]()
      while (!isAtEnd) {
        start = current
        val token: Option[Token] = scanToken()
        token.foreach(tokens.append)
      }

      tokens.append(Token(EOF, "", None, line))
      tokens.toSeq
    } finally {
      start = 0
      current = 0
      line = 1
    }
  }

  private def isAtEnd: Boolean = current >= source.length

  private def scanToken(): Option[Token] = {
    val c: Char = getAndAdvance()
    c match {
      case '(' => Some(createToken(LEFT_PAREN))
      case ')' => Some(createToken(RIGHT_PAREN))
      case '{' => Some(createToken(LEFT_BRACE))
      case '}' => Some(createToken(RIGHT_BRACE))
      case ',' => Some(createToken(COMMA))
      case '.' => Some(createToken(DOT))
      case '-' => Some(createToken(MINUS))
      case '+' => Some(createToken(PLUS))
      case ';' => Some(createToken(SEMICOLON))
      case ':' => Some(createToken(COLON))
      case '*' => Some(createToken(STAR))
      case '?' => Some(createToken(QUESTION_MARK))
      case '!' => Some(createToken(if (matchAtCurrent('=')) BANG_EQUAL else BANG))
      case '=' => Some(createToken(if (matchAtCurrent('=')) EQUAL_EQUAL else EQUAL))
      case '<' => Some(createToken(if (matchAtCurrent('=')) LESS_EQUAL else LESS))
      case '>' => Some(createToken(if (matchAtCurrent('=')) GREATER_EQUAL else GREATER))
      case '/' => if (matchAtCurrent('/')) {
        //comment
        while (peek != '\n' && !isAtEnd) {
          advance()
        }
        None
      } else if (matchAtCurrent('*')) {
        matchMultiLineComment()
        None
      } else {
        Some(createToken(SLASH))
      }
      case '"' => matchString()
      case c if c.isDigit => Some(matchNumber())
      case c if isAlpha(c) => Some(matchIdentifier())
      case c if c.isWhitespace =>
        if (c == '\n') {
          line += 1
        }
        None

      case _ => ErrorHandler.error(line, s"Unexpected character $c")
        None

    }
  }

  private def createToken[L](tokenType: TokenType, literal: Option[L] = None): Token = {
    val text: String = source.substring(start, current)
    Token(tokenType, text, literal, line)
  }

  private def getAndAdvance(): Char = {
    val c = source.charAt(current)
    advance()
    c
  }

  private def advance(): Unit = {
    current += 1
  }

  private def peek: Char = {
    if (isAtEnd) {
      EOF_CHAR
    } else {
      source.charAt(current)
    }
  }

  private def peekNext: Char = {
    if (current + 1 >= source.length) {
      EOF_CHAR
    } else {
      source.charAt(current + 1)
    }
  }

  private def matchAtCurrent(expected: Char): Boolean = {
    if (isAtEnd || source.charAt(current) != expected) {
      false
    } else {
      current += 1
      true
    }
  }

  private def matchString(): Option[Token] = {
    while (peek != '"' && !isAtEnd) {
      if (peek == '\n') {
        line += 1
      }
      advance()
    }

    if (isAtEnd) {
      ErrorHandler.error(line, "Unterminated string.")
      None
    } else {
      advance()
      val value: String = source.substring(start + 1, current - 1)
      Some(createToken(STRING, Some(value)))
    }
  }

  private def matchNumber(): Token = {
    while (peek.isDigit) {
      advance()
    }

    if (peek == '.' && peekNext.isDigit) {
      advance()
      while (peek.isDigit) {
        advance()
      }
    }

    createToken(NUMBER, Some(source.substring(start, current).toDouble))
  }

  private def matchIdentifier(): Token = {
    while (isAlphaNumeric(peek)) {
      advance()
    }

    val text = source.substring(start, current)
    val tokenType: TokenType = RESERVED_KEYWORDS.getOrElse(text, IDENTIFIER)

    createToken(tokenType)
  }

  private def matchMultiLineComment(): Unit = {
    val multiLineCommentStartLine = line
    var nestingLevel = 1
    while (nestingLevel > 0) {
      while (!(peek == '*' && peekNext == '/') && !isAtEnd) {
        if (peek == '\n') {
          line += 1
        } else if (peek == '/' && peekNext == '*') {
          nestingLevel += 1
          advance() //advance by another 1 to prevent /*/ from being a legal comment
        }
        advance()
      }
      if (!isAtEnd) {
        //consume both * and /
        advance()
        advance()
        nestingLevel -= 1
      } else {
        ErrorHandler.error(multiLineCommentStartLine, "Unterminated multi-line comment.")
        nestingLevel = 0
      }
    }
  }

  private def isAlpha(c: Char): Boolean = c.isLetter || c == '_'

  private def isAlphaNumeric(c: Char): Boolean = c.isLetterOrDigit || c == '_'
}

object Scanner {
  val EOF_CHAR = '\u0000'
  val RESERVED_KEYWORDS: Map[String, TokenType] = Seq(AND, CLASS, ELSE, FALSE, FOR, FUN, IF, NIL, OR, PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE)
    .map(t => t.name().toLowerCase -> t)
    .toMap
}
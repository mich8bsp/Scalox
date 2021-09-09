package com.github.scalox

object ErrorHandler {

  var silent = false
  var hadError = false
  var hadRuntimeError = false

  def error(line: Int, message: String, where: String = ""): Unit = {
    if(!silent){
      System.err.println(s"[line $line] Error$where: $message")
    }
    hadError = true
  }

  def error(token: Token, message: String): Unit = {
    val where: String = if(token.tokenType == TokenType.EOF){
      " at end"
    }else{
      s" at '${token.lexeme}'"
    }
    error(line = token.line, message = message, where = where)
  }

  def runtimeError(e: RuntimeError): Unit = {
    if(!silent){
      System.err.println(s"${e.getMessage}\n[line ${e.token.line}]")
    }
    hadRuntimeError = true
  }

  def reset(): Unit = {
    silent = false
    hadError = false
    hadRuntimeError = false
  }
}

class ParseError extends RuntimeException
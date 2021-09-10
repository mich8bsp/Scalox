package com.github.scalox

import scala.io.{Source, StdIn}

object Lox {
  private val interpreter = new Interpreter

  def runFile(path: String): Unit = {
    val source = Source.fromFile(path)
    try {
      val fileContent: String = source.getLines().mkString("\n")
      run(fileContent)

      if (ErrorHandler.hadError) {
        System.exit(65)
      }
      if (ErrorHandler.hadRuntimeError) {
        System.exit(70)
      }
    } finally {
      source.close()
    }
  }

  def runPrompt(): Unit = {
    var line = StdIn.readLine()
    while (line != null) {
      run(line)
      ErrorHandler.reset()
      line = StdIn.readLine()
    }
  }

  private def run(source: String): Unit = {
    val scanner: Scanner = new Scanner(source)
    val tokens: Seq[Token] = scanner.scanTokens()
    val parser: Parser = new Parser(tokens)

    ErrorHandler.silent = true
    val parsedAsStatementsSuccess: Boolean = parser.parseStatements().exists(_ => !ErrorHandler.hadError)
    ErrorHandler.reset()
    if (!parsedAsStatementsSuccess) {
      parser.parseExpression().foreach(ast => interpreter.interpret(ast))
    } else {
      parser.parseStatements().filter(_ => !ErrorHandler.hadError) match {
        case Nil =>
        case ast => interpreter.interpret(ast)
      }
    }
  }


  def main(args: Array[String]): Unit = args match {
    case Array() => runPrompt()
    case Array(script) => runFile(script)
    case _ => println("Usage: scalox [script]")
      System.exit(64)
  }
}

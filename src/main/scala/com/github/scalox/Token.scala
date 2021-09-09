package com.github.scalox

case class Token(
                  tokenType: TokenType,
                  lexeme: String,
                  literal: Option[Any],
                  line: Int
                ) {

  override def toString: String = s"$tokenType $lexeme ${literal.map(_.toString).getOrElse("")}"
}

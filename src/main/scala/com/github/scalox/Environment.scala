package com.github.scalox

import scala.collection.mutable

class Environment(enclosingEnv: Option[Environment] = None) {
  private val values: mutable.Map[String, Option[Any]] = mutable.Map[String, Option[Any]]()

  def define(name: Token, value: Option[Any]): Unit = {
    if (isDefined(name.lexeme)) {
      throw RuntimeError(name, s"Variable '${name.lexeme}' is already defined in this scope.")
    }
    values.put(name.lexeme, value)
  }

  def isDefined(name: String): Boolean = {
    values.contains(name) || enclosingEnv.exists(_.isDefined(name))
  }

  def get(name: Token): Option[Any] = {
    values.getOrElse(name.lexeme,
      enclosingEnv.map(_.get(name))
        .getOrElse(throw RuntimeError(name, s"Undefined variable '${name.lexeme}'."))
    )
  }

  def assign(name: Token, value: Option[Any]): Unit = {
    if (values.contains(name.lexeme)) {
      values.put(name.lexeme, value)
    } else {
      enclosingEnv match {
        case Some(parentEnv) => parentEnv.assign(name, value)
        case None => throw RuntimeError(name, s"Undefined variable '${name.lexeme}'.")
      }
    }
  }
}

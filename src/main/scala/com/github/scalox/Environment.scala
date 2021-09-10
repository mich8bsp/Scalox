package com.github.scalox

import scala.collection.mutable

class Environment(enclosingEnv: Option[Environment] = None) {
  private val values: mutable.Map[String, Option[Any]] = mutable.Map[String, Option[Any]]()

  def define(name: Token, value: Option[Any],
             checkNotDefinedInCurrentScope: Boolean = true,
             checkNotDefinedInParentScopes: Boolean = true): Unit = {
    if (checkNotDefinedInCurrentScope && isDefined(name.lexeme, checkNotDefinedInParentScopes)) {
      throw RuntimeError(name, s"Variable '${name.lexeme}' is already defined in this scope.")
    }
    define(name.lexeme, value)
  }

  def define(name: String, value: Option[Any]): Unit = {
    values.put(name, value)
  }

  private def isDefined(name: String,
                        checkNotDefinedInParentScopes: Boolean): Boolean = {
    values.contains(name) ||
      (checkNotDefinedInParentScopes &&
        enclosingEnv.exists(_.isDefined(name, checkNotDefinedInParentScopes)))
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

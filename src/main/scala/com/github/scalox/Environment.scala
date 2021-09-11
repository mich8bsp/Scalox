package com.github.scalox

import scala.collection.mutable

class Environment(val enclosingEnv: Option[Environment] = None) {
  private val values: mutable.Map[String, Option[Any]] = mutable.Map[String, Option[Any]]()

  def define(name: Token, value: Option[Any],
             checkNotDefinedInCurrentScope: Boolean = true): Unit = {
    if (checkNotDefinedInCurrentScope && isDefined(name.lexeme)) {
      throw RuntimeError(name, s"Variable '${name.lexeme}' is already defined in this scope.")
    }
    define(name.lexeme, value)
  }

  def define(name: String, value: Option[Any]): Unit = {
    values.put(name, value)
  }

  private def isDefined(name: String): Boolean = {
    values.contains(name)
  }

  def get(name: Token): Option[Any] = {
    values.getOrElse(name.lexeme,
      enclosingEnv.map(_.get(name))
        .getOrElse(throw RuntimeError(name, s"Undefined variable '${name.lexeme}'."))
    )
  }

  def getAt(name: Token, distance: Int): Option[Any] = {
    if(distance == 0){
      values.getOrElse(name.lexeme, throw RuntimeError(name, s"Undefined variable '${name.lexeme}'."))
    }else{
      ancestor(distance) match {
        case Some(relevantEnv) => relevantEnv.getAt(name, 0)
        case None => throw new Exception("Invalid compiler state.")
      }
    }
  }

  def getAt(name: String, distance: Int): Option[Any] = {
    if(distance == 0){
      values(name)
    }else{
      ancestor(distance) match {
        case Some(relevantEnv) => relevantEnv.getAt(name, 0)
        case None => throw new Exception("Invalid compiler state.")
      }
    }
  }

  def assignAt(name: Token, distance: Int, value: Option[Any]): Unit = {
    ancestor(distance) match {
      case Some(relevantEnv) => relevantEnv.define(name, value)
      case None => throw new Exception("Invalid compiler state.")
    }
  }

  private def ancestor(distance: Int): Option[Environment] = {
    var env: Option[Environment] = Some(this)
    (0 until distance).foreach(_ => env = env.flatMap(_.enclosingEnv))
    env
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

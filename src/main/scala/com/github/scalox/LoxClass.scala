package com.github.scalox

import scala.collection.mutable

class LoxClass(name: String,
               methods: Map[String, LoxFunction]) extends LoxCallable {

  override def toString: String = name

  override def arity(): Int = {
    findMethod("init")
      .map(_.arity())
      .getOrElse(0)
  }

  override def call(arguments: Seq[Option[Any]])
                   (implicit interpreter: Interpreter): Option[Any] = {
    val instance: LoxInstance = new LoxInstance(this)
    val initializer: Option[LoxFunction] = findMethod("init")
    initializer.foreach(_.bind(instance).call(arguments))
    Some(instance)
  }

  def findMethod(name: String): Option[LoxFunction] = {
    methods.get(name)
  }
}

class LoxInstance(clazz: LoxClass){

  private val fields: mutable.Map[String, Option[Any]] = mutable.Map()

  def get(name: Token): Option[Any] = {
    val fieldOpt: Option[Option[Any]] = fields.get(name.lexeme)
    lazy val methodOpt: Option[Option[Any]] = clazz.findMethod(name.lexeme)
      .map(_.bind(this))
      .map(Some(_))

    fieldOpt
      .orElse(methodOpt)
      .getOrElse(throw RuntimeError(name, s"Undefined property '${name.lexeme}'."))
  }

  def set(name: Token, value: Option[Any]): Unit = {
    fields.put(name.lexeme, value)
  }

  override def toString: String = s"${clazz.toString} instance"
}

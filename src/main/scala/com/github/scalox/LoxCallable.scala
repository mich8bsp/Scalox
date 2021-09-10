package com.github.scalox

trait LoxCallable {
  def arity(): Int


  def call(arguments: Seq[Option[Any]])
          (implicit interpreter: Interpreter): Option[Any]
}

class LoxFunction(name: Option[String],
                  declaration: FunctionExpr,
                  closure: Environment) extends LoxCallable {
  override def arity(): Int = declaration.parameters.size

  override def call(arguments: Seq[Option[Any]])
                   (implicit interpreter: Interpreter): Option[Any] = {
    val functionEnv: Environment = new Environment(Some(closure))
    declaration.parameters.zip(arguments).foreach({
      case (paramToken, argValue) =>
        //defining a parameter with same name as already defined in outer scope variable is allowed
        functionEnv.define(paramToken, argValue, checkNotDefinedInParentScopes = false)
    })
    try{
      declaration.body.foreach(stmt => interpreter.execute(stmt)(functionEnv))
      None
    } catch {
      case e: ReturnException => e.value
    }
  }

  override def toString: String = name.map(x => s"<fn $x>").getOrElse("<fn>")
}
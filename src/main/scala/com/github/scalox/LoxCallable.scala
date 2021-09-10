package com.github.scalox

trait LoxCallable {
  def arity(): Int


  def call(arguments: Seq[Option[Any]])
          (implicit interpreter: Interpreter): Option[Any]
}

class LoxFunction(declaration: FunctionStmt) extends LoxCallable {
  override def arity(): Int = declaration.params.size

  override def call(arguments: Seq[Option[Any]])
                   (implicit interpreter: Interpreter): Option[Any] = {
    val functionEnv: Environment = new Environment(Some(interpreter.globalEnv))
    declaration.params.zip(arguments).foreach({
      case (paramToken, argValue) =>
        //defining a parameter with same name as already defined in outer scope variable is allowed
        functionEnv.define(paramToken, argValue, checkNotDefinedInParentScopes = false)
    })
    declaration.body.foreach(stmt => interpreter.execute(stmt)(functionEnv))
    None
  }

  override def toString: String = s"<fn ${declaration.name.lexeme}>"
}
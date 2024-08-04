package br.unb.cic.flang

import StateMonad._

case class FDeclaration(name: String, arg: String, body: Expr)

object Declarations {
  def lookup(
      name: String,
      declarations: List[FDeclaration]
  ): MState[FDeclaration] = declarations match {
    case List() => ???
    case (f@FDeclaration(n, a, b))::_ if n == name => pure(f)
    case _::fs => lookup(name, fs)  
  }

}

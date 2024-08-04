package br.unb.cic.flang

import Declarations._
import StateMonad._
import cats.data.State

object Interpreter {

  def eval(expr: Expr, declarations: List[FDeclaration]): MState[Integer] =
    expr match {
      case CInt(v) => pure(v)
      case Add(lhs, rhs) => for {
        l <- eval(lhs, declarations)
        r <- eval(rhs, declarations)
      } yield l + r
      case Mul(lhs, rhs) => for {
        l <- eval(lhs, declarations)
        r <- eval(rhs, declarations)
      } yield l * r
      case Id(name) => for {
        state <- State.get[S]
        res <- pure(lookupVar(name, state))
      } yield res
      case App(name, arg) => for {
        fdecl <- lookup(name, declarations)
        value <- eval(arg, declarations)
        s1 <- State.get[S]
        s2 <- State.set(declareVar(fdecl.arg, value, s1))
        res <- eval(fdecl.body, declarations)
      } yield res
    }
}

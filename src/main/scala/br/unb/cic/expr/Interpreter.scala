package br.unb.cic.flang

import MErrState._

import Declarations._
import Substitution._
import cats.data.EitherT
import cats.data.State

object Interpreter {
  def eval(expr: Expr, declarations: List[FDeclaration]): MStateEH[Integer] = expr match {
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
      state <- EitherT.right(State.get[S])
      res <- lookupVar(name, state)
    } yield res
    case App(name, arg) => for {
      fdecl <- lookup(name, declarations)
      value <- eval(arg, declarations) 
      s1 <- EitherT.right(State.get[S])
      s2 <- EitherT.right(State.set(declareVar(fdecl.arg, value, s1)))
      res <- eval(fdecl.body, declarations)
    } yield res
  }
}

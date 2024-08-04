package br.unb.cic.flang

import cats.data.State

package object StateMonad {
  type S = List[(String, Integer)]

  type MState[A] = State[S,A]
  def pure[A](a: A): MState[A] = State.pure(a) 

  def declareVar(name: String, value: Integer, state: S): S =
    (name, value) :: state

  def lookupVar(name: String, state: S): Integer = state match {
    case List()                      => ???
    case (n, v) :: tail if n == name => v
    case _ :: tail                   => lookupVar(name, tail)
  }

}

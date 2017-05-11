package dottyfun

import BetterFailables._

object Main {
  def main(args: Array[String]): Unit = {
    println{
      recover[Handle = E1](
        op1.flatMap(op2),
        {(e: E1) => "Error1 Averted!"}
      )

      // op1.flatMap(op2).rec {e: E1 =>
      //   "Error1 Averted"
      // }
    }
  }
}

object BetterFailables {
  sealed trait Failable[+E, +A] {
    def flatMap[E1, B](f: A => Failable[E1, B]): Failable[E | E1, B]

    def map[B](f: A => B): Failable[E, B] =
      flatMap(f andThen Success[E, B])
  }

  sealed case class Failure[E, A](e: E) extends Failable[E, A] {
    def flatMap[E1, B](f: A => Failable[E1, B]): Failable[E | E1, B] =
      this.asInstanceOf[Failable[E | E1, B]]
  }

  sealed case class Success[E, A](a: A) extends Failable[E, A] {
    def flatMap[E1, B](f: A => Failable[E1, B]): Failable[E | E1, B] =
      f(a)
  }

  // implicit class FailableHandle[Handle, Rest, A](failable: Failable[Handle | Rest, A]) {
  //   def rec(f: Handle => A): Failable[Rest, A] = null
  // }

  def recover[Handle, E2, A](failable: Failable[Handle | E2, A], rec: Handle => A): Failable[E2, A] = null

  trait E1
  trait E2

  def op1: Failable[E1, String] = Success("op1")

  def op2(param: String): Failable[E2, String] = Success("op2")
}

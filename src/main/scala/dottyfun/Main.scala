package dottyfun

import scala.reflect._
import BetterFailables._

object Main {
  def main(args: Array[String]): Unit = {
    case class E1()
    case class E2()

    def op1: Failable[E1, String] = Success("op1")
    // def op1: Failable[E1, String] = Failure(E1())

    // def op2(param: String): Failable[E2, String] = Success("op2")
    def op2(param: String): Failable[E2, String] = Failure(E2())

    def combinedOps = op1 flatMap op2

    // combinedOps.value // won't compile

    val stillFailable = recover[Handle = E1](combinedOps) {e: E1 => "E1 recovered!"}

    // stillFailable.value  // won't compile

    val unfailable = recover[Handle = E2](stillFailable) {e: E2 => "E2 recovered!"}

    println(unfailable.value)
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

  def recover[Handle : ClassTag, E2, A](failable: Failable[Handle | E2, A])(rec: Handle => A): Failable[E2, A] = failable match {
    case s: Success[_, _] =>
      s.asInstanceOf[Failable[E2, A]]
    case f: Failure[_, _] =>
      if (ClassTag(f.e.getClass) == implicitly[ClassTag[Handle]])
        Success[E2, A](rec(f.e.asInstanceOf[Handle]))
      else
        failable.asInstanceOf[Failable[E2, A]]
  }

  implicit class UnfailableValue[A](unfailable: Failable[Nothing, A]) {
    def value: A = unfailable.asInstanceOf[Success[Nothing, A]].a
  }
}

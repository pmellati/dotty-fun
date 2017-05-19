package dottyfun

import scala.reflect._

case class T1()
case class T2()
case class T3()

object TypeErasure {
  case class Parametrised[T](t: T)

  case class ParametrisedTagged[T](t: T, ctag: ClassTag[T]) // Dotty doesn't support TypeTags. Since we have an instance of `T`, there is no reason to get the `ClassTag` here.

  def parametrisedTagged[T : ClassTag](t: T): ParametrisedTagged[T] =
    ParametrisedTagged[T](t, implicitly)

  def main(args: Array[String]): Unit = {
    val nonParametrised: T1 | T2 | T3 = T2()

    nonParametrised match {
      case _: T1 => println("T1")
      case _: T2 => println("T2")
      case _: T3 => println("T3")
    }

    val pt = parametrisedTagged(T2())

    pt.ctag match {
      case t if t == classTag[T1] => println("PT[T1]")
      case t if t == classTag[T2] => println("PT[T2]")
      case t if t == classTag[T3] => println("PT[T3]")
    }

    val p = Parametrised(T2())
    ClassTag(p.t.getClass) match {
      case t if t == classTag[T1] => println("P[T1]")
      case t if t == classTag[T2] => println("P[T2]")
      case t if t == classTag[T3] => println("P[T3]")
    }
  }
}

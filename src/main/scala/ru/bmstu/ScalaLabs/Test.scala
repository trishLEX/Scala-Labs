package ru.bmstu.ScalaLabs

abstract class Adder[T] {
  def sum(a: T, b: T): T
}

object Adder {
  implicit object int extends Adder[Int] {
    override def sum(a: Int, b: Int): Int = a + b
  }

  implicit object str extends Adder[String] {
    override def sum(a: String, b: String): String = a + b
  }
}

class Test[T](val a: T, val b: T) {
  def sum[T: Adder](a: T, b: T): T = implicitly[Adder[T]].sum(a, b)
}

object Main {
  def main(args: Array[String]): Unit = {
    val test = new Test[Int](1, 2)
    println(test.sum(test.a, test.b))
  }
}

package ru.bmstu.ScalaLabs

object Lab1 extends App {
  val reverse: (List[Int], List[Int], Int => Boolean) => List[Int] = {
    case (Nil, res, _) => res
    case (x :: xs, res, p) if p(x) => reverse(xs, x :: res, p)
    case (x :: xs, res, p) => reverse(xs, res, p)
  }

  val reverseP: (List[Int], Int => Boolean) => List[Int] = {
    case (xs, p) => reverse(xs, Nil, p)
  }

  val list = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  val res = reverseP(list, _ % 2 == 0)
  print(res)
}

package ru.bmstu.ScalaLabs

abstract class Numeric[T] {
  def kadane(list: List[T]): T
}

object KadaneStack{
  implicit object short extends Numeric[Short] {
    override def kadane(list: List[Short]): Short = list.scanLeft(0)((acc, n) => math.max(0, acc + n)).max.asInstanceOf[Short]
  }

  implicit object int extends Numeric[Int] {
    override def kadane(list: List[Int]): Int = list.scanLeft(0)((acc, n) => math.max(0, acc + n)).max
  }

  implicit object long extends Numeric[Long] {
    override def kadane(list: List[Long]): Long = list.scanLeft(0L)((acc, n) => math.max(0, acc + n)).max
  }

  implicit object float extends Numeric[Float] {
    override def kadane(list: List[Float]): Float = list.scanLeft(0.0f)((acc, n) => math.max(0, acc + n)).max
  }

  implicit object double extends Numeric[Double] {
    override def kadane(list: List[Double]): Double = list.scanLeft(0.0)((acc, n) => math.max(0, acc + n)).max
  }
}

class KadaneStack[T] private (val stack: List[T], private val max: List[T]) {
  def this() = this(Nil, Nil)

  private def pushA[T: Numeric](stack: List[T]): KadaneStack[T] = {
    val newMax = implicitly[Numeric[T]].kadane(stack) :: max
    new KadaneStack[T](stack, newMax.asInstanceOf[List[T]])
  }

  def push[T](x: T): KadaneStack[T] = {
    implicit val num = x
    if (x.isInstanceOf[Int])
      pushA[T]((x :: this.stack).asInstanceOf[List[T]])
    else
      pushNotNum[T](x)
    //push((x :: this.stack).asInstanceOf[List[T]])
  }

  private def pushNotNum[T](x: T): KadaneStack[T] = {
    val newStack = x :: this.stack
    new KadaneStack[T](newStack.asInstanceOf[List[T]], Nil)
  }

  def maxSum = this.max.head

  def pop() = (stack.head, new KadaneStack(stack.drop(1), max.drop(1)))
  def empty() = stack.isEmpty

  override def toString: String = (stack, max).toString()
}

object Lab4 {
  def main(args: Array[String]): Unit = {
    var stack = new KadaneStack[Int]()
    println(stack)
    stack = stack.push(1)
    println(stack)
    stack = stack.push(2)
    println(stack)
    stack = stack.push(-1)
    println(stack)
    println(stack.maxSum)

    var strStack = new KadaneStack[String]()
    strStack = strStack.push("A")
    println(strStack)
    println(strStack.maxSum)
  }
}

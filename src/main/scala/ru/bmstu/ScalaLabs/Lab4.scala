package ru.bmstu.ScalaLabs

abstract class NumericMaxSum[T] {
  def maxSum(stack: List[(T, T)]): T
}

object KadaneStack {
  def pushInt[Int](x: Int, stack: List[(Int, Int)])(implicit num: Numeric[Int]) = new KadaneStack(
    (x, (x :: stack.map(_._1))
      .scanLeft(num.zero)((acc, n) => num.max(num.zero, num.plus(acc, n))).max
    ) :: stack
  )

  def pushShort[Short](x: Short, stack: List[(Short, Short)])(implicit num: Numeric[Short]) = new KadaneStack(
    (x, (x :: stack.map(_._1))
      .scanLeft(num.zero)((acc, n) => num.max(num.zero, num.plus(acc, n))).max
    ) :: stack
  )

  def pushLong[Long](x: Long, stack: List[(Long, Long)])(implicit num: Numeric[Long]) = new KadaneStack(
    (x, (x :: stack.map(_._1))
      .scanLeft(num.zero)((acc, n) => num.max(num.zero, num.plus(acc, n))).max
    ) :: stack
  )

  def pushDouble[Double](x: Double, stack: List[(Double, Double)])(implicit num: Numeric[Double]) = new KadaneStack(
    (x, (x :: stack.map(_._1))
      .scanLeft(num.zero)((acc, n) => num.max(num.zero, num.plus(acc, n))).max
    ) :: stack
  )

  def pushFloat[Flaot](x: Float, stack: List[(Float, Float)])(implicit num: Numeric[Float]) = new KadaneStack(
    (x, (x :: stack.map(_._1))
      .scanLeft(num.zero)((acc, n) => num.max(num.zero, num.plus(acc, n))).max
    ) :: stack
  )

  def push[T](x: T, stack: List[(T, T)]) = new KadaneStack[T]((x, x) :: stack)
}

object NumericMaxSum {
  implicit object int extends NumericMaxSum[Int] {
    override def maxSum(stack: List[(Int, Int)]): Int = stack.head._2
  }

  implicit object short extends NumericMaxSum[Short] {
    override def maxSum(stack: List[(Short, Short)]): Short = stack.head._2
  }

  implicit object long extends NumericMaxSum[Long] {
    override def maxSum(stack: List[(Long, Long)]): Long = stack.head._2
  }

  implicit object double extends NumericMaxSum[Double] {
    override def maxSum(stack: List[(Double, Double)]): Double = stack.head._2
  }

  implicit object float extends NumericMaxSum[Float] {
    override def maxSum(stack: List[(Float, Float)]): Float = stack.head._2
  }
}

class KadaneStack[T] private(val stack: List[(T, T)]) {
  def this() = this(Nil)

  def push(x: T): KadaneStack[T] = {
    //new KadaneStackNew((x, null) :: stack)
    x match {
      case _: Int    => KadaneStack.pushInt(x.asInstanceOf[Int], stack.asInstanceOf[List[(Int, Int)]]).asInstanceOf[KadaneStack[T]]
      case _: Short  => KadaneStack.pushShort(x.asInstanceOf[Short], stack.asInstanceOf[List[(Short, Short)]]).asInstanceOf[KadaneStack[T]]
      case _: Long   => KadaneStack.pushLong(x.asInstanceOf[Long], stack.asInstanceOf[List[(Long, Long)]]).asInstanceOf[KadaneStack[T]]
      case _: Float  => KadaneStack.pushFloat(x.asInstanceOf[Float], stack.asInstanceOf[List[(Float, Float)]]).asInstanceOf[KadaneStack[T]]
      case _: Double => KadaneStack.pushDouble(x.asInstanceOf[Double], stack.asInstanceOf[List[(Double, Double)]]).asInstanceOf[KadaneStack[T]]
      case _         => KadaneStack.push(x, stack)
    }
  }

  def pop() = new KadaneStack(stack.tail)

  def maxSum()(implicit num: NumericMaxSum[T]) = num.maxSum(stack)

  override def toString: String = stack.toString()
}

object Lab4 {
  def main(args: Array[String]): Unit = {
    var stack = new KadaneStack[Int]()
    println(stack)
    stack = stack.push(1)
    println(stack)
    stack = stack.push(2)
    println(stack)
    stack = stack.push(-11)
    //println(stack)
    stack = stack.push(10)
    println(stack.maxSum())

    var strStack = new KadaneStack[String]()
    strStack = strStack.push("A")
    println(strStack)

    var floatStack = new KadaneStack[Float]()
    floatStack = floatStack.push(1.0f)
    floatStack = floatStack.push(2.0f)
    floatStack = floatStack.push(-10.0f)
    println(floatStack)
    println(floatStack.maxSum())
  }
}

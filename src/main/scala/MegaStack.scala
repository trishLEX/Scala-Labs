import scala.Numeric.Implicits._

object MegaStackImplicits{
  implicit def toAverage[T: Numeric](megaStack: MegaStack[T]): MegaStackAverage[T] = new MegaStackAverage[T]()
}

abstract class MegaStackAbstract[T: Numeric] {
  def average(list: List[T]): T
}

//object MegaStack {
//  implicit object num extends
//}

class MegaStack[T] private(val stack: List[T]){

  def this() = this(Nil)

  def empty(): Boolean = stack.isEmpty

  def push(element: T) = new MegaStack[T](element :: stack)

  def pop(): (T, MegaStack[T]) = (stack.head, new MegaStack[T](stack.tail))

}

class MegaStackAverage[T: Numeric] private(val stack: List[(T, Double)]){

  def this() = this(Nil)

  def empty(): Boolean = stack.isEmpty

  def average(): Double = stack.head._2

  def push(element: T): MegaStackAverage[T] = stack match {
    case Nil => new MegaStackAverage[T]((element, element.toDouble()) :: Nil)
    case notNil => new MegaStackAverage[T]((element, element.toDouble() + notNil.map(_._1).sum.toDouble() / (stack.length + 1)) :: stack)
  }

  def pop(): (T, MegaStackAverage[T]) = (stack.head._1, new MegaStackAverage[T](stack.tail))
}
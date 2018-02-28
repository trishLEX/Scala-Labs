package ru.bmstu.ScalaLabs

import ru.bmstu.ScalaLabs.ElmntType.ElmntType

object ElmntType extends Enumeration {
  type ElmntType = Value
  val A, B, invA, invB, E = Value
}

class Element(x: ElmntType) {
  val symbol = x

  override def toString: String = symbol match {
    case ElmntType.A    => "a"
    case ElmntType.B    => "b"
    case ElmntType.invA => "ã"
    case ElmntType.invB => "ƀ"
    case ElmntType.E    => "e"
  }

  def unary_- () =
    symbol match {
      case ElmntType.A    => new Element(ElmntType.invA)
      case ElmntType.B    => new Element(ElmntType.invB)
      case ElmntType.invA => new Element(ElmntType.A)
      case ElmntType.invB => new Element(ElmntType.B)
      case ElmntType.E    => new Element(ElmntType.E)
  }

  def - (other: Element) = this + (- other)

  def + (other: Element) =
    (this.symbol, other.symbol) match {
      case (ElmntType.A, ElmntType.invA) => new Word(List(new Element(ElmntType.E)))
      case (ElmntType.B, ElmntType.invB) => new Word(List(new Element(ElmntType.E)))
      case (ElmntType.invA, ElmntType.A) => new Word(List(new Element(ElmntType.E)))
      case (ElmntType.invB, ElmntType.B) => new Word(List(new Element(ElmntType.E)))
      case (elmnt, ElmntType.E)          => new Word(List(new Element(elmnt)))
      case (ElmntType.E, elmnt)          => new Word(List(new Element(elmnt)))
      case _                             => new Word(List(this, other))
  }
}

class Word(x: List[Element]) {
  val word = x.filter(p => p.symbol.id != 4)

  override def toString: String = word.mkString

  def + (other: Element): Word = new Word(if (word.nonEmpty) word.reverse.tail.reverse ::: (word.last + other).word
                                     else List(other))

  def + (other: Word): Word = {
    def recurs(xs: List[Element], res: List[Element]):Word = (xs, res) match {
      case (Nil, res) => new Word(res)
      case (x :: xs, res) => recurs(xs, (new Word(res) + x).word)
    }

    recurs(other.word, word)
  }

  def unary_- () = new Word(word.reverse.map(a => -a))

  def - (other: Element) = this + (-other)
}

object Lab2 {

  def main(args: Array[String]): Unit = {
    val a = new Element(ElmntType.A)
    val notA = new Element(ElmntType.invA)
    val b = new Element(ElmntType.B)
    val notB = new Element(ElmntType.invB)
    val e = new Element(ElmntType.E)


    println(a + a + notA)
    println(a + b)
    println(a + b + notA)
    println((a + b) + notB)
    println(-(notA + b) + (notA + a)) //ƀaãa = ƀa
    println(- b + b + a)
    println(-(b+a) + (b + a) + a)
    println(e + a - b + (b + a) - a + e)
  }
}

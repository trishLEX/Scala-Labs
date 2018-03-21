package ru.bmstu.ScalaLabs

object Lab3 {
  class Pos private(val prog: String, val offs: Int, val line: Int, val col: Int) {
    def this(prog: String) = this(prog, 0, 1, 1)
    def ch = if ( offs == prog.length ) -1 else prog.codePointAt(offs)
    def inc = ch match {
      case '\n' => new Pos(prog, offs + 1, line + 1, 1)
      case -1   => this
      case _    => new Pos(prog, offs + 1, line, col + 1)
    }

    override def toString = "(" + line + " , " + col + ")"
  }

  object DomainTags extends Enumeration {
    type Tag = Value
    val ERROR, WHITESPACE, END_OF_PROGRAM, STRING, NUMBER, IDENTIFIER = Value
  }

  import ru.bmstu.ScalaLabs.Lab3.DomainTags._

  class Scanner {
    //def scan(start: Pos): (Tag, Pos) = sys.error("syntax error at " + start)
    def scan(start: Pos): (Tag, Pos) = (ERROR, start.inc)
  }

  class Token(val start: Pos, scanner: Scanner) {
    val (tag, follow) = start.ch match {
      case -1 => (END_OF_PROGRAM, start)
      case _  => scanner.scan(start)
    }

    def image = start.prog.substring(start.offs, follow.offs)
    def next = new Token(follow, scanner)
  }

  trait Whitespaces extends Scanner {
    private def missWhitespace(pos: Pos): Pos = pos.ch match {
      case ' '  => missWhitespace(pos.inc)
      case '\t' => missWhitespace(pos.inc)
      case '\n' => missWhitespace(pos.inc)
      case _    => pos
    }

    override def scan(start: Pos): (Tag, Pos) = {
      val follow = missWhitespace(start)
      if (start != follow) (WHITESPACE, follow)
      else super.scan(start)
    }
  }

  trait Strings extends Scanner {
    private def missString(pos: Pos): (Pos, Boolean) = pos.ch match {
      case '\'' => if (pos.inc.ch == '\'') missString(pos.inc.inc) else (pos.inc, false)
      case '\n' => println("RETURN pos: " + pos); (pos, true)
      case -1   => (pos, true)
      case _    => missString(pos.inc)
    }

    override def scan(start: Pos): (Tag, Pos) = {
      if (start.ch == '\'') {
        val (follow, error) = missString(start.inc)
        if (!error) (STRING, follow)
        else super.scan(start)
      } else super.scan(start)
    }
  }

  trait Idents extends Scanner {
    private def missIdent(pos: Pos): Pos = pos.ch match {
      case '.' if pos.inc.ch.toChar.isLetter => missIdent(pos.inc)
      case x   if x.toChar.isLetterOrDigit   => missIdent(pos.inc)
      case _                                 => pos
    }

    override def scan(start: Pos): (Tag, Pos) = {
      if (start.ch.toChar.isLetter) {
        val follow = missIdent(start.inc)
        (IDENTIFIER, follow)
      } else super.scan(start)
    }
  }

  trait Numbers extends Scanner {

    private def missNumber(pos: Pos, wasPoint: Boolean): (Pos, Boolean) = pos.ch match {
      case '.' if pos.inc.ch.toChar.isDigit => if (wasPoint) (pos, true) else missNumber(pos.inc, wasPoint = true)
      case x => if (x.toChar.isDigit) missNumber(pos.inc, wasPoint) else (pos, false)
    }

    override def scan(start: Pos): (Tag, Pos) = {
      if (start.ch.toChar.isDigit || start.ch == '-') {
        val (follow, error) = missNumber(start.inc, wasPoint = false)
        if (!error)
          (NUMBER, follow)
        else super.scan(start)
      } else super.scan(start)
    }
  }

  def main(args: Array[String]): Unit = {
    var t = new Token(
      new Pos("'hello''' -2.0 this.world"),
      new Scanner with Strings with Numbers with Idents with Whitespaces
    )

    while (t.tag != END_OF_PROGRAM) {
      println(t.tag.toString + " " + t.start + "-" + t.follow + ": " + t.image)
      t = t.next
    }
  }
}

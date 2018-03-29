object Main {
  def main(args: Array[String]): Unit = {
    //testInt()
    //testString()

    var stack = new MegaStack[Int]()
    stack = stack.push(1)
    stack = stack.push(2)
    //println(stack.average())
  }

  def testInt(): Unit = {
    println("test")
    var stack = new MegaStackAverage[Int]()
    stack = stack.push(0)
    stack = stack.push(1)
    stack = stack.push(2)
    stack = stack.push(13)
    while (!stack.empty()){
      println("Average: " + stack.average())
      val (element, st) = stack.pop()
      stack = st
      println("Element: " + element)
    }
  }

  def testString() = {
    println("test")
    var stack = new MegaStack[String]()
    stack = stack.push("1")
    stack = stack.push("2")
    stack = stack.push("3")
    stack = stack.push("4")
    while (!stack.empty()){
      //      println("Average: " + stack.average())
      val (element, st) = stack.pop()
      stack = st
      println("Element: " + element)
    }
  }

}

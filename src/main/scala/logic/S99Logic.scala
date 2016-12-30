package logic

class S99Logic(x: Boolean) {
  import S99Logic._

  def and(y: Boolean): Boolean = {
    (x, y) match {
      case (true, true) => true
      case _ => false
    }
  }

  def or(y: Boolean): Boolean = {
    (x, y) match {
      case (true, _) => true
      case (_, true) => true
      case _ => false
    }
  }

  def eq(y: Boolean): Boolean = {
    (x and y) or (not(x) and not(y))
  }

  def nand(x: Boolean, y: Boolean): Boolean = {
    not(x and y)
  }

  def nor(x: Boolean, y: Boolean): Boolean = {
    not(x or y)
  }

  def xor(x: Boolean, y: Boolean): Boolean = {
    not(x eq y)
  }

  def impl(x: Boolean, y: Boolean): Boolean = {
    not(x) or y
  }
}
object S99Logic {
  implicit def boolean2S99Logic(y: Boolean): S99Logic = new S99Logic(y)

  def not(x: Boolean): Boolean = {
    !x
  }

  def table2(function: (Boolean, Boolean) => Boolean): Unit = {
    println("A\t\t\tB\t\t\tresult")
    for (x <- List(true, false);
         y <- List(true, false)) {
      println(x + "\t\t" + y + "\t\t" + function(x, y))
    }
  }
}

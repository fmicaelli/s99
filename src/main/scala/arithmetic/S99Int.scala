package arithmetic

import scala.collection.immutable.Stream.Cons

class S99Int(val start: Int) {
  import S99Int._
  def isPrime: Boolean = {
    (start > 1) && (primes takeWhile { x => x * x <= start } forall { start % _ != 0 })
  }

  def isCoprimeTo(otherInt: Int): Boolean = {
    gcd(start, otherInt) == 1
  }
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
  val primes: Cons[Int] = Stream.cons(2, Stream.from(3, 2) filter { _.isPrime })
  def gcd(m: Int, n: Int): Int = {
    if (n == 0) {
      m
    } else {
      gcd(n, m % n)
    }
  }
}

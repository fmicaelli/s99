package arithmetic

import scala.collection.immutable.Stream.Cons

class S99Int(val start: Int) {

  import S99Int._

  def isPrime: Boolean = {
    (start > 1) && (primes takeWhile { x => x * x <= start } forall {
      start % _ != 0
    })
  }

  def isCoprimeTo(otherInt: Int): Boolean = {
    gcd(start, otherInt) == 1
  }

  def totient(): Int = {
    Range.inclusive(1, start).count(_.isCoprimeTo(start))
  }

  def primeFactors(): List[Int] = {
    def primeFactors(number: Int, primes: Stream[Int]): List[Int] = {
      if (number.isPrime) {
        List(number)
      } else if (number % primes.head == 0) {
        primes.head :: primeFactors(number / primes.head, primes)
      } else {
        primeFactors(number, primes.tail)
      }
    }

    primeFactors(start, primes)
  }

  def primeFactorMultiplicity(): List[(Int, Int)] = {
    def primeFactorMultiplicity(number: Int,
                                primes: Stream[Int],
                                count: Int,
                                factors: List[(Int, Int)]): List[(Int, Int)] = {
      if (number.isPrime) {
        if (number == primes.head) {
          factors ::: List((primes.head, count + 1))
        }
        else {
          factors ::: List((primes.head, count), (number, 1))
        }
      } else if (number % primes.head == 0) {
        primeFactorMultiplicity(number / primes.head, primes, count + 1, factors)
      } else {
        primeFactorMultiplicity(number, primes.tail, 0, if (count > 0) factors ::: List((primes.head, count)) else factors)
      }
    }

    primeFactorMultiplicity(start, primes, 0, List())
  }

  def goldbach(): (Int, Int) = {
    def goldbach(primes1: Stream[Int], primes2: Stream[Int]): (Int, Int) = {
      if (primes1.head + primes2.head == start) {
        (primes1.head, primes2.head)
      } else if (primes1.head + primes2.head > start) {
        goldbach(primes1.tail, primes1.tail)
      } else {
        goldbach(primes1, primes2.tail)
      }
    }

    goldbach(primes, primes)
  }
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

  val primes: Cons[Int] = Stream.cons(2, Stream.from(3, 2) filter {
    _.isPrime
  })

  def gcd(m: Int, n: Int): Int = {
    if (n == 0) {
      m
    } else {
      gcd(n, m % n)
    }
  }

  def phi(m: Int): Int = {
    m.primeFactorMultiplicity()
      .map(x => (x._1 - 1) * scala.math.pow(x._1, x._2 - 1)).product.toInt
  }

  def listPrimesinRange(range: Range): List[Int] = {
    range.filter(_.isPrime).toList
  }

  def printGoldbachList(range: Range): Unit = {
    range.filter(_ % 2 == 0)
      .foreach(x => {
        val goldbach = x.goldbach()
        println(x + " = " + goldbach._1 + " + " + goldbach._2)
      })
  }

  def printGoldbachListLimited(range: Range, limit: Int): Unit = {
    range.filter(n => n > 2 && n % 2 == 0)
      .filter(_.goldbach()._1 > limit)
      .foreach(x => {
        val goldbach = x.goldbach()
        println(x + " = " + goldbach._1 + " + " + goldbach._2)
      })
  }
}

package Exercise

object Exercise extends App {
  def last(list: List[Any]): Any = {
    if (list.tail.isEmpty) {
      list.head
    } else {
      last(list.tail)
    }
  }

  def penultimate(list: List[Any]): Any = {
    def penultimate(head: Any, list: List[Any]): Any = {
      if (list.tail.isEmpty) {
        head
      } else {
        penultimate(list.head, list.tail)
      }
    }
    penultimate(list.head, list.tail)
  }

  def nth(index: Int, list: List[Any]): Any = {
    def nth(current: Int, index: Int, list: List[Any]): Any = {
      if (current == index) {
        list.head
      } else {
        nth(current + 1, index, list.tail)
      }
    }
    nth(0, index, list)
  }

  def length(list: List[Any]): Any = {
    def length(currentLength: Int, list: List[Any]): Any = {
      if (list.isEmpty) {
        currentLength
      } else {
        length(currentLength + 1, list.tail)
      }
    }
    length(0, list)
  }

  def reverse(list: List[Any]): Any = {
    def reverse(currentList: List[Any], list: List[Any]): List[Any] = {
      if (list.isEmpty) {
        currentList
      } else {
        reverse(list.head :: currentList, list.tail)
      }
    }

    reverse(List(), list)
  }

  def isPalindrome(list: List[Any]): Boolean = {
    list == reverse(list)
  }

  def flatten(list: List[Any]): List[Any] = {
    list match {
      case Nil => Nil
      case (head: List[_]) :: tail => flatten(head) ::: flatten(tail)
      case head :: tail => head :: flatten(tail)
    }
  }

  def compress(list: List[Any]): List[Any] = {
    def compress(current: Any, currentList: List[Any], list: List[Any]): List[Any] = {
      if (list.isEmpty) {
        currentList ::: List(current)
      } else if (list.head.equals(current)) {
        compress(current, currentList, list.tail)
      } else {
        compress(list.head, currentList ::: List(current), list.tail)
      }
    }
    compress(list.head, List(), list.tail)
  }

  def pack(list: List[Any]): List[List[Any]] = {
    def pack(finalList: List[List[Any]], currentList: List[Any], list: List[Any]): List[List[Any]] = {
      if (list.isEmpty) {
        finalList ::: List(currentList)
      } else {
        if (currentList.head == list.head) {
          pack(finalList, currentList ::: List(list.head), list.tail)
        } else {
          pack(finalList ::: List(currentList), List(list.head), list.tail)
        }
      }
    }
    pack(List(), List(list.head), list.tail)
  }

  def encode(list: List[Any]): List[(Int, Any)] = {
    pack(list).map(x => (x.size, x.head))
  }

  def encodeModified(list: List[Any]): List[Any] = {
    pack(list).map(x => if (x.size > 1) (x.size, x.head) else x.head)
  }

  def decode(list: List[(Int, Any)]): List[Any] = {
    def extract(tuple: (Int, Any)): List[Any] = {
      val array = for (i <- 0 to tuple._1) yield tuple._2
      array.toList
    }
    list.flatMap(extract)
  }

  def encodeDirect(list: List[Any]): List[(Int, Any)] = {
    def encodeDirect(finalList: List[(Int, Any)], current: (Int, Any), list: List[Any]): List[(Int, Any)] = {
      if (list.isEmpty) {
        finalList ::: List((current._1 + 1, current._2))
      } else {
        if (list.head == current._2) {
          encodeDirect(finalList, (current._1 + 1, current._2), list.tail)
        } else {
          encodeDirect(finalList ::: List(current), (1, list.head), list.tail)
        }
      }
    }
    encodeDirect(List(), (1, list.head), list.tail)
  }

  def duplicate(list: List[Any]): List[Any] = {
    list.flatMap(x => List(x, x))
  }

  def duplicateN(count: Int, list: List[Any]): List[Any] = {
    def extract(count: Int, current: Any): List[Any] = {
      val array = for (i <- 0 until count) yield current
      array.toList
    }
    list.flatMap(extract(count, _))
  }

  def drop(multiple: Int, list: List[Any]): List[Any] = {
    list.zipWithIndex
      .filter(x =>(x._2 + 1) % multiple != 0)
      .map(_._1)
  }

  def split(index: Int, list: List[Any]): (List[Any], List[Any]) = {
    (list.take(index), list.drop(index))
  }

  def slice(from: Int, to: Int, list: List[Any]): List[Any] = {
    list.drop(from).take(to - from)
  }

  def rotate(number: Int, list: List[Any]): List[Any] = {
    if (number > 0) {
      rotate(number - 1, list.tail ::: List(list.head)) }
    else if (number < 0) {
      rotate(number + 1, list.last :: list.dropRight(1))
    } else {
      list
    }
  }

  def removeAt(index: Int, list: List[Any]): (List[Any], Any) = {
    (list.take(index) ::: list.takeRight(list.size - index - 1), list(index))
  }

  def insertAt(element: Any, index: Int, list: List[Any]): List[Any] = {
    list.take(index) ::: List(element) ::: list.takeRight(list.size - index)
  }

  def range(from: Int, to: Int): List[Int] = {
    (from to to).toList
  }

  print(range(4, 9))

}
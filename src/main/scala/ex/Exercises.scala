package ex

object Exercises {

  def last[A](list: List[A]): A = list match {
    case h :: Nil => h
    case _ :: tail => last(tail)
    case _ => throw new NoSuchElementException
  }

  def penultimate[A](list: List[A]): A = list match {
    case head :: last :: Nil => head
    case head :: tail => penultimate(tail)
    case _ => throw new NoSuchElementException
  }

  def nth[A](idx: Int, list: List[A]): A = idx match {
    case 0 => list.head
    case x => nth(x - 1, list.tail)
  }

  def length[A](list: List[A]): Int = list match {
    case Nil => 0
    case head :: tail => 1 + length(tail)
  }

  def reverse[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case head :: tail => reverse(tail) ::: head :: Nil
  }

  def isPalindrome[A](list: List[A]): Boolean = list == list.reverse

  def flatten(list: List[Any]): List[Any] = list match {
    case Nil => Nil
    case head :: tail => {
      head match {
        case aList: List[Any] => flatten(aList)
        case anElement: Any => anElement :: Nil
      }
    } ::: flatten(tail)
  }

  def compress(list: List[Symbol]): List[Symbol] = list match {
    case Nil => Nil
    case last :: Nil => last :: Nil
    case head :: tail if head == tail.head => compress(tail)
    case head :: tail => head :: compress(tail)
  }

  def pack(list: List[Symbol]): List[List[Symbol]] = list match {
    case Nil => Nil
    case head :: tail => List(head :: tail.takeWhile(_ == head)) ::: pack(tail.dropWhile(_ == head))
  }

  def encodeDirect(list: List[Symbol]): List[(Int, Symbol)] = list match {
    case Nil => Nil
    case head :: tail => List((tail.takeWhile(_ == head).size + 1, head)) ::: encodeDirect(tail.dropWhile(_ == head))
  }

  def encode(list: List[Symbol]): List[(Int, Symbol)] = pack(list).map {
    e => (e.size, e.head)
  }

  def encodeModified(list: List[Symbol]): List[Any] = list match {
    case Nil => Nil
    case head :: tail => tail.takeWhile(_ == head).size match {
      case 0 => head :: encodeModified(tail)
      case size: Int => List((size + 1, head)) ::: encodeModified(tail.dropWhile(_ == head))
    }
  }

  def decode[A](ls: List[(Int, A)]): List[A] = ls flatMap {
    e => List.fill(e._1)(e._2)
  }

  def duplicate(list: List[Symbol]): List[Symbol] = list match {
    case Nil => Nil
    case head :: tail => head :: head :: duplicate(tail)
  }

  def duplicateN(times: Int, list: List[Symbol]): List[Symbol] = list match {
    case Nil => Nil
    case head :: tail => List.fill(times)(head) ::: duplicateN(times, tail)
  }

  def drop(i: Int, list: List[Symbol]): List[Symbol] = {
    def aux(list: List[Symbol], ctr: Int): List[Symbol] = (ctr, list) match {
      case (_, Nil) => Nil
      case (1, head :: tail) => aux(tail, i)
      case (c, head :: tail) => head :: aux(tail, c - 1)
    }
    aux(list, i)
  }

  //TODO remove drop call
  def split(n: Int, list: List[Symbol]) = {
    def aux(list: List[Symbol], c: Int): List[Symbol] = (c, list) match {
      case (0, head :: tail) => Nil
      case (n, head :: tail) => head :: aux(tail, c - 1)
    }
    val firstPart = aux(list, n)
    (firstPart, list.drop(firstPart.length))
  }

  def slice[A](from: Int, to: Int, list: List[A]): List[A] = (from, list) match {
//   def aux(from: Int, to: Int, list: List[A]): List[A] = {
//    case (c, head :: tail) =>
//      if(c )
//      slice(c + 1, to, tail)
//  }
    case _ => Nil
  }



}

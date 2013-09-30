package exc

object Exercises {

  def last[A](list: List[A]): A = list match {
    case h :: Nil  => h
    case _ :: tail => last(tail)
    case _         => throw new NoSuchElementException
  }

  def penultimate[A](list: List[A]): A = list match {
    case head :: last :: Nil => head
    case head :: tail        => penultimate(tail)
    case _                   => throw new NoSuchElementException
  }

  def nth[A](idx: Int, list: List[A]): A = idx match {
    case 0        => list.head
    case x        => nth(x - 1, list.tail)
  }

  def length[A](list: List[A]): Int = list match {
    case Nil             => 0
    case head :: tail    => 1 + length(tail)
  }

  def reverse[A](list: List[A]): List[A] = list match {
    case Nil             =>  Nil
    case head :: tail    => reverse(tail) ::: head :: Nil
  }

  def isPalindrome[A](list: List[A]): Boolean = list == list.reverse

  def flatten(list: List[Any]): List[Any] = list match {
    case Nil => Nil
    case head :: tail => { head match {
      case aList: List[Any] => flatten(aList)
      case anElement: Any => anElement :: Nil
    }} ::: flatten(tail)
  }

  def compress(list: List[Symbol]): List[Symbol] = list match {
    case Nil => Nil
    case last :: Nil => last :: Nil
    case head :: tail if head == tail.head => compress(tail)
    case head :: tail                      => head :: compress(tail)
  }

  def pack(list: List[Symbol]): List[Any] = list match {
    case Nil => Nil
    case head :: tail => List(head :: tail.takeWhile(_ == head)) ::: pack(tail.dropWhile(_ == head))
  }

  def encode(list: List[Symbol]): List[(Int, Symbol)] = list match {
    case Nil => Nil
    case head :: tail => List((tail.takeWhile(_ == head).size + 1, head)) ::: encode(tail.dropWhile(_ == head))
  }

}

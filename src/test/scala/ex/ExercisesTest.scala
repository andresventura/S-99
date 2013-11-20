package ex

import org.scalatest.{BeforeAndAfter, FunSuite}

class ExercisesTest extends FunSuite with BeforeAndAfter {

  test("Test P01") {
    assert(Exercises.last(List(1, 1, 2, 3, 5, 8)) === 8)
  }

  test("Test P02") {
    assert(Exercises.penultimate(List(1, 1, 2, 3, 5, 8)) === 5)
  }

  test("Test P03") {
    assert(Exercises.nth(2, List(1, 1, 2, 3, 5, 8)) === 2)
  }

  test("Test P04") {
    assert(Exercises.length(List(1, 1, 2, 3, 5, 8)) === 6)
  }

  test("Test P05") {
    assert(Exercises.reverse(List(1, 2, 3, 4, 5, 6)) === List(6, 5, 4, 3, 2, 1))
  }

  test("Test P06") {
    assert(Exercises.isPalindrome(List(1, 2, 3, 2, 1)) === true)
  }

  test("Test P07") {
    assert(Exercises.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) === List(1, 1, 2, 3, 5, 8))
  }

  test("Test P08") {
    assert(Exercises.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List('a, 'b, 'c, 'a, 'd, 'e))
    assert(Exercises.compress(Nil) === Nil)
  }

  test("Test P09") {
    assert(Exercises.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ===
      List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
    assert(Exercises.pack(List('a)) ===
      List(List('a)))
  }

  test("Test P10") {
    assert(Exercises.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ===
      List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  test("Test P11") {
    assert(Exercises.encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ===
      List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
  }

  test("Test P12") {
    assert(Exercises.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) ===
      List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

  test("Test P13") {
    assert(Exercises.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ===
      List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }

  test("Test P14") {
    assert(Exercises.duplicate(List('a, 'b, 'c, 'c, 'd)) ===
      List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }

  test("Test P15") {
    assert(Exercises.duplicateN(3, List('a, 'b, 'c, 'c, 'd)) ===
      List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }

  test("Test P16") {
    assert(Exercises.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ===
      List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  test("Test P17") {
    assert(Exercises.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ===
      (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }

  ignore("Test P18") {
    assert(Exercises.slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) ===
      List('d, 'e, 'f, 'g))
  }


}

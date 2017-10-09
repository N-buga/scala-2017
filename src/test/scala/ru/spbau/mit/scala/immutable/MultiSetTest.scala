package ru.spbau.mit.scala.immutable

import org.scalatest.{BeforeAndAfterEach, FunSuite}

class MultiSetTest extends FunSuite with BeforeAndAfterEach {
  val simpleImmutableMultiSet = NotEmptyNode(1, 2, NotEmptyNode(6, 1, EmptyNode))

  val multiSet: MultiSet[Int] = MultiSet(1, 2, 4, 1, 2, 1, 3, 7)
  val multiSet2 = MultiSet(1)
  val multiSet3 = MultiSet(6)

  test("testApplyObject") {
    assert(multiSet == NotEmptyNode(1, 3, NotEmptyNode(2, 2, NotEmptyNode(4, 1, NotEmptyNode(3, 1, NotEmptyNode(7, 1, EmptyNode))))))
  }

  test("testUnapplySeq") {
    MultiSet(1, 2) match {
      case MultiSet(1, 2) =>
      case _ => fail()
    }
  }

  test("for-comprehension") {
    var cnt_1 = 0
    var cnt_2 = 0
    var cnt_3 = 0
    var cnt_6 = 0
    for (v <- multiSet) {
      v match {
        case 1 => cnt_1 += 1
        case 2 => cnt_2 += 1
        case 3 => cnt_3 += 1
        case 6 => cnt_6 += 1
        case _ =>
      }
    }

    assert(cnt_1 == 3)
    assert(cnt_2 == 2)
    assert(cnt_3 == 1)
    assert(cnt_6 == 0)
  }

  test("testApplyClass") {
    assert(multiSet(1) == 3)
    assert(multiSet(6) == 0)
  }

  test("testForEach") {
    var cnt_1 = 0
    multiSet.foreach(el => if (el == 1) cnt_1 += 1)
    assert(cnt_1 == 3)

    var cnt_6 = 0
    multiSet.foreach(el => if (el == 6) cnt_6 += 1)
    assert(cnt_6 == 0)
  }

  test("or") {
    val union = multiSet2 | multiSet3
    assert(union(1) == 1)
    assert(union(6) == 1)

    val union2 = union | multiSet2
    assert(union2(1) == 2)
    assert(union(1) == 1)
  }

  test("and") {
    val andRes = simpleImmutableMultiSet & multiSet
    assert(andRes.depth == 1)
    assert(andRes.size == 2)
    assert(andRes(1) == 2)
    assert(andRes(6) == 0)

    assert(multiSet.depth == 5)
    assert(simpleImmutableMultiSet.depth == 2)
  }

  test("multi") {
    val newImmutableMultiSet = multiSet*2
    assert(newImmutableMultiSet(1) == 6)
    assert(multiSet(1) == 3)
  }

  test("add") {
    val newImmutableMultiSet = multiSet + 1
    assert(newImmutableMultiSet(1) == 4)
    assert(multiSet(1) == 3)
  }

  test ("testOrSimple") {
    val union = simpleImmutableMultiSet | simpleImmutableMultiSet
    assert(union.apply(1) == 4)
    assert(union.apply(6) == 2)

    assert(simpleImmutableMultiSet.apply(1) == 2)
    assert(simpleImmutableMultiSet.apply(6) == 1)
  }

  test("testRemoveSimple") {
    val (cnt, removedImmutableMultiSet) = simpleImmutableMultiSet.remove(1)
    assert(cnt == 2)
    assert(removedImmutableMultiSet(1) == 0)
    assert(removedImmutableMultiSet(6) == 1)

    assert(simpleImmutableMultiSet.apply(1) == cnt)
  }

  test("testRemove") {
    val (cnt, newMultiset) = multiSet.remove(1)
    assert(cnt == 3)
    assert(newMultiset(1) == 0)
    assert(multiSet(1) == 3)
  }

  test("testFilter") {
    val newImmutableMultiSet = multiSet.filter(el => el <= 2)
    assert(newImmutableMultiSet.depth == 2)
    assert(newImmutableMultiSet.size == 5)
    assert(newImmutableMultiSet(3) == 0)
  }

  test("testFlatMap") {
    val newImmutableMultiSet: MultiSet[Int] = multiSet.flatMap((el: Int) => MultiSet(el, el + 1))
    assert(newImmutableMultiSet(1) == 3)
    assert(newImmutableMultiSet(2) == 5)

    assert(multiSet(2) == 2)
  }

  test("testDepth") {
    assert(multiSet.depth == 5)
  }

  test("testSize") {
    assert(multiSet.size == 8)
  }

  test("testFind") {
    assert(multiSet.find(1).contains(1))
    assert(!multiSet.find(6).contains(6))
  }

  test("testMap") {
    val newImmutableMultiSet: MultiSet[String] = multiSet.map(_.toString)
    assert(newImmutableMultiSet.apply("1") == 3)
  }

}

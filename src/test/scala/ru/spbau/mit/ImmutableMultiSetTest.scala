package ru.spbau.mit

import org.scalatest.{BeforeAndAfterEach, FunSuite}

class ImmutableMultiSetTest extends FunSuite with BeforeAndAfterEach {
  val simpleMultiSet = NoEmptyNode(1, 2, NoEmptyNode(6, 1, EmptyNode))

  val multiSet = ImmutableMultiSet(1, 2, 4, 1, 2, 1, 3, 7)
  val multiSet2 = ImmutableMultiSet(1)
  val multiSet3 = ImmutableMultiSet(6)

  test("testApplyObject") {
    assert(multiSet == NoEmptyNode(1, 3, NoEmptyNode(2, 2, NoEmptyNode(4, 1, NoEmptyNode(3, 1, NoEmptyNode(7, 1, EmptyNode))))))
  }

  test("testUnapplySeq") {
    ImmutableMultiSet(1, 2) match {
      case ImmutableMultiSet(1, 2) => assert(true)
      case _ => assert(false)
    }
  }

  test("for-comprehension") {
    var cnt_1 = 0
    var cnt_2 = 0
    var cnt_3 = 0
    var cnt_6 = 0
    for (v <- multiSet) {
      if (v == 1) {
        cnt_1 += 1
      }
      if (v == 2) {
        cnt_2 += 1
      }
      if (v == 3) {
        cnt_3 += 1
      }
      if (v == 6) {
        cnt_6 += 1
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
    val andRes = simpleMultiSet & multiSet
    assert(andRes.depth == 1)
    assert(andRes.size == 2)
    assert(andRes(1) == 2)
    assert(andRes(6) == 0)

    assert(multiSet.depth == 5)
    assert(simpleMultiSet.depth == 2)
  }

  test("multi") {
    val newMultiSet = multiSet*2
    assert(newMultiSet(1) == 6)
    assert(multiSet(1) == 3)
  }

  test("add") {
    val newMultiSet = multiSet + 1
    assert(newMultiSet(1) == 4)
    assert(multiSet(1) == 3)
  }

  test ("testOrSimple") {
    val union = simpleMultiSet | simpleMultiSet
    assert(union.apply(1) == 4)
    assert(union.apply(6) == 2)

    assert(simpleMultiSet.apply(1) == 2)
    assert(simpleMultiSet.apply(6) == 1)
  }

  test("testRemoveSimple") {
    val (cnt, removedMultiSet) = simpleMultiSet.remove(1)
    assert(cnt == 2)
    assert(removedMultiSet(1) == 0)
    assert(removedMultiSet(6) == 1)

    assert(simpleMultiSet.apply(1) == cnt)
  }

  test("testRemove") {
    val (cnt, newMultiset) = multiSet.remove(1)
    assert(cnt == 3)
    assert(newMultiset(1) == 0)
    assert(multiSet(1) == 3)
  }

  test("testFilter") {
    val newMultiSet = multiSet.filter(el => el <= 2)
    assert(newMultiSet.depth == 2)
    assert(newMultiSet.size == 5)
    assert(newMultiSet(3) == 0)
  }

  test("testFlatMap") {
    val newMultiSet = multiSet.flatMap(el => ImmutableMultiSet(el, el + 1))
    assert(newMultiSet(1) == 3)
    assert(newMultiSet(2) == 5)

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
    val newMultiSet = multiSet.map(el => el.toString)
    assert(newMultiSet.isInstanceOf[ImmutableMultiSet[String]])
    assert(newMultiSet.apply("1") == 3)
  }

}

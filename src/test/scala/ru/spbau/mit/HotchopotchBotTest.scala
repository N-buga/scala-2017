package ru.spbau.mit

import org.scalatest.FunSuite
import ru.spbau.mit.parser.{GetClassMethodsMessage, MessageParser}

class HotchopotchBotTest extends FunSuite {

  test("Methods") {
    assertResult(
      """scala.util.matching.Regex#findAllIn
        |scala.util.matching.Regex#anchored
        |scala.util.matching.Regex#findAllIn
        |scala.util.matching.Regex#findAllMatchIn
        |scala.util.matching.Regex#findFirstIn
        |scala.util.matching.Regex#findFirstMatchIn
        |scala.util.matching.Regex#findPrefixMatchOf
        |scala.util.matching.Regex#findPrefixOf
        |scala.util.matching.Regex#pattern
        |scala.util.matching.Regex#regex
        |scala.util.matching.Regex#replaceAllIn
        |scala.util.matching.Regex#replaceAllIn
        |scala.util.matching.Regex#replaceFirstIn
        |scala.util.matching.Regex#replaceSomeIn
        |scala.util.matching.Regex#runMatcher
        |scala.util.matching.Regex#split
        |scala.util.matching.Regex#toString
        |scala.util.matching.Regex#unanchored
        |scala.util.matching.Regex#unapplySeq
        |scala.util.matching.Regex#unapplySeq
        |scala.util.matching.Regex#unapplySeq
        |scala.util.matching.Regex#unapplySeq""".stripMargin) {
        MessageParser.parse("Methods scala.util.matching.Regex") match {
          case msg: GetClassMethodsMessage => msg.getMethods
          case _ => "Fail"
        }
    }
  }

  test("Wrong class name") {
    assertResult("Wrong class name") {
      MessageParser.parse("Methods scala.util.mathcing.Regexp") match {
        case msg: GetClassMethodsMessage => msg.getMethods
        case _ => "Fail"
      }
    }
  }
}

package ru.spbau.mit

import org.scalatest.FunSuite
import ru.spbau.mit.parser.{GetClassMethodsMessage, HistoryMessage, MessageParser, WrongMessage}

class MessageParserTest extends FunSuite {

  test("Methods") {
    assertResult(GetClassMethodsMessage("scala.util.matching.Regex")) {
      MessageParser.parse("Methods scala.util.matching.Regex")
    }
  }

  test("methods") {
    assertResult(GetClassMethodsMessage("scala.util.matching.Regex")) {
      MessageParser.parse("methods scala.util.matching.Regex")
    }
  }

  test("History") {
    assertResult(HistoryMessage()) {
      MessageParser.parse("History")
    }
  }

  test("history") {
    assertResult(HistoryMessage()) {
      MessageParser.parse("history")
    }
  }

  test("Wrong class name") {
    assertResult(GetClassMethodsMessage("scala.util.matching.Regexp")) {
      MessageParser.parse("Methods scala.util.matching.Regexp")
    }
  }

  test("Wrong message") {
    assert(MessageParser.parse("Blabla").isInstanceOf[WrongMessage])
  }
}
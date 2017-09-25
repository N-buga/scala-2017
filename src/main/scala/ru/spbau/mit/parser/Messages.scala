package ru.spbau.mit.parser

import java.io.FileNotFoundException

trait UserMessage

case class EchoMessage(message: String) extends UserMessage

case class GetClassMethodsMessage(message: String) extends UserMessage {
  def getMethods:String = {
    val words = message.split("\\.")
    val urlPart = words.mkString("/")
    try {
      val content = io.Source.fromURL(s"http://www.scala-lang.org/api/2.12.3/$urlPart.html").mkString
      val pattern = s"$message\\#([a-zA-z]+)".r
      val methods = pattern.findAllIn(content).matchData.mkString("\n")
      methods
    } catch {
      case e: FileNotFoundException =>
        "Wrong class name"
    }
  }
}

case class HistoryMessage() extends UserMessage {}

case class WrongMessage(message: String) extends UserMessage

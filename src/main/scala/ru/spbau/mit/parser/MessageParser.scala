package ru.spbau.mit.parser

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class MessageParser extends RegexParsers {
    override def skipWhitespace = true

    override val whiteSpace: Regex = "[ \t\r\f]+".r

    val wordParser: Parser[String] = raw"\S+".r

    val echoParser: Parser[EchoMessage] = "[Ee]cho".r ~> wordParser ^^ (message => EchoMessage(message))
    val classParser: Parser[GetClassMethodsMessage] = "[Mm]ethods".r ~> wordParser ^^
      (clazz => GetClassMethodsMessage(clazz))
    val historyParser: Parser[HistoryMessage] = "[Hh]istory\\.?".r ^^ (_ => HistoryMessage())

    val userMessage: Parser[UserMessage] = echoParser | classParser | historyParser
}

object MessageParser extends MessageParser{
    def parse(text: String): UserMessage = {
        parse(userMessage, text) match {
            case Success(message, _) => message
            case other: NoSuccess =>
                WrongMessage(other.msg)
        }
    }
}
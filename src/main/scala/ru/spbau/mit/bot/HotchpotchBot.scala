package ru.spbau.mit.bot

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import ru.spbau.mit.database.RequestActor.{GetClassMethodsRequest, HistoryRequest}
import ru.spbau.mit.parser._

import scala.concurrent.duration.DurationInt
import scala.util.Success


class HotchpotchBot(val token: String, val database: ActorRef) extends TelegramBot with Polling with Commands {

  onMessage(
    implicit message =>
      message.text.foreach(
        text =>
          MessageParser.parse(text) match {
            case EchoMessage(msg) =>
              reply(msg)
            case GetClassMethodsMessage(msg) =>
              database ! GetClassMethodsRequest(message.chat.id, msg)
              reply(GetClassMethodsMessage(msg).getMethods)
            case msg: HistoryMessage =>
              implicit val timeout: Timeout = Timeout(1.second)
              (database ? HistoryRequest(message.chat.id)).onComplete{
                case Success(history) => reply(history.toString)
                case _ => reply("Database error")
              }
            case WrongMessage(msg) =>
              reply(msg)
          }
      )
  )
}

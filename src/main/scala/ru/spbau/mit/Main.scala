package ru.spbau.mit

import akka.actor.{ActorSystem, Props}
import ru.spbau.mit.bot.HotchpotchBot
import ru.spbau.mit.database.RequestActor

object Main {
  def main(args: Array[String]): Unit = {
    val token = "423626163:AAH20GiuEZyOuVIVGdoNqWmhecj3Cp1aRzk"

    val system = ActorSystem()
    val database = system.actorOf(Props(classOf[RequestActor]))
    val bot = new HotchpotchBot(token, database)

    bot.run()
  }
}

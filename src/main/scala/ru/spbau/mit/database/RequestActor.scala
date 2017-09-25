package ru.spbau.mit.database

import akka.persistence.PersistentActor

import scala.collection.mutable

class RequestActor extends PersistentActor {
  import RequestActor._

  val history: mutable.HashMap[Long, mutable.Queue[String]] = mutable.HashMap.empty

  def receiveEvent(event: Event): Unit = {
    event match {
      case GetClassMethodsRequest(id, message) =>
        history.getOrElseUpdate(id, new mutable.Queue[String]()) += message
        if (history(id).size > 10)
          history(id).dequeue()
    }
  }

  override def receiveRecover: Receive = {
    case evt: Event => receiveEvent(evt)
  }

  override def receiveCommand: Receive = {
    case evt: Event => persist(evt)(receiveEvent)
    case HistoryRequest(id) =>
      val currentHistory = history.getOrElseUpdate(id, mutable.Queue.empty).mkString("\n")
      sender ! currentHistory
  }

  override def persistenceId = "Request database"
}

object RequestActor {
  trait Event

  case class GetClassMethodsRequest(id: Long, query: String) extends Event

  case class HistoryRequest(id: Long)
}
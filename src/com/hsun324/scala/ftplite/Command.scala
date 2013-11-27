package com.hsun324.scala.ftplite

import scala.actors.Actor
import scala.concurrent._
import scala.reflect.ClassTag
import scala.actors.FutureActor
import scala.actors.OutputChannel

object Dependent
abstract class Command[+T](protected val client: Client) extends Actor {
  var dependent: Option[OutputChannel[Any]] = None
  override def exceptionHandler = {
    case e: Exception => dependent foreach {_ ! e}
  }
  def act() {
  	loop {
    	react {
    	  case Dependent => dependent = Some(sender)
        case Response(code, message) => handleResponse(code, message) match {
          case Some(response) => dependent foreach {_ ! response}; reply(true); exit()
          case _ => reply(false)
        }
    	}
  	}
  }
  def execute()
  def handleResponse(code: Int, message: Seq[String]): Option[FutureResult[T]]
  def name: String
}
class ConnectCommand(c: Client) extends Command[Unit](c) {
  def execute() { }
  def handleResponse(code: Int, message: Seq[String]) = {
    c.state <~ State.Username
    Some(FutureResult(true))
  }
  def name = "CONNECT"
}
final class ChainedCommand[T](protected val commands: IndexedSeq[Command[T]], c: Client) extends Command[T](c) {
  if (commands.length == 0) throw new IllegalArgumentException()
  private var currentCommand = 0
  private var currentResult = FutureResult[T](true)
  def execute() = commands(currentCommand).execute()
  def handleResponse(code: Int, message: Seq[String]): Option[FutureResult[T]] = commands(currentCommand).handleResponse(code, message) match {
      case Some(response) =>
        currentResult &&= response
        currentCommand += 1
        if (currentResult.successful && currentCommand < commands.length) {
          commands(currentCommand).execute()
          None
        } else Some(currentResult)
      case _ => None
    }
  def name = commands(currentCommand).name
}
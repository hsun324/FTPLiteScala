package com.hsun324.scala.ftplite

import java.net.InetAddress
import java.net.ServerSocket

abstract class ModeCommand[+T](c: Client) extends TextCommand[T](c) {
  protected object AsInt {
    def unapply(str: String) = util.Try(Some(str.toInt)) getOrElse None
  }
  def active: Boolean
}
object ModeCommand {
  trait ModeCommandFactory {
    def apply[T](c: Client): ModeCommand[T]
    def active: Boolean
  }
  
  object PassiveModeCommandFactory extends ModeCommandFactory {
    def apply[T](c: Client) = new PassiveModeCommand[T](c)
    def active = false
  }
  object ActiveModeCommandFactory extends ModeCommandFactory {
    def apply[T](c: Client) = new ActiveModeCommand[T](c)
    def active = true
  }
}
class ActiveModeCommand[+T](c: Client) extends ModeCommand[T](c) {
  lazy val port = (1000 to 1050) find {port => util.Try(new ServerSocket(port).close()).isSuccess} getOrElse 1024
  lazy val address = InetAddress.getLocalHost.getAddress map {_.toString} reduceLeft {_+","+_}
  def text = "PORT " + address + "," + (port / 256) + "," + (port % 256)
  def handleResponse(code: Int, message: Seq[String]) = Some(if (code == 200) {
    c.state <~ State.Read
    c.dataPort = port
    FutureResult(true)
  } else FutureResult(false))
  def active = false
  def name = "PORT"
}
class PassiveModeCommand[+T](c: Client) extends ModeCommand[T](c) {
  protected val extendedRegex = ".*?\\((.)\\1{2}([0-9]{1,5})\\1\\).*".r
  protected val regex = "[^(]*\\(((?:[0-9]{1,3},?){4}),([0-9]{1,3}),([0-9]{1,3})\\)".r
  def text = if (c.features ? Feature.ExtendedPassive) "EPSV" else "PASV"
  def handleResponse(code: Int, message: Seq[String]) = Some(if (code == 229 || code == 227) {
    message(0) match {
      case extendedRegex(_, AsInt(port)) => 
        c.state <~ State.Read
        c.dataHost = c.host
        c.dataPort = port
        println(">> " + port)
        FutureResult(true)
      case regex(host, AsInt(portU), AsInt(portL)) =>
        c.state <~ State.Read
        c.dataHost = host.replace(',', '.')
        c.dataPort = portU * 256 + portL
        FutureResult(true)
      case _ => FutureResult(false)
    }
  } else FutureResult(false))
  def active = false
  def name = "PASV"
}
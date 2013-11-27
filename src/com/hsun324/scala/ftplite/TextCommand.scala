package com.hsun324.scala.ftplite

abstract class TextCommand[+T](c: Client) extends Command[T](c) {
  def execute() = c.send(text)
  def text(): String
}
class UsernameCommand(username: String, c: Client) extends TextCommand[Unit](c) {
  def text = "USER " + username
  def handleResponse(code: Int, message: Seq[String]) = Some(if (code == 331) {
      c.state <~ State.Password
      FutureResult(true)
    } else FutureResult(false))
  def name = "USER"
}
class PasswordCommand(password: String, c: Client) extends TextCommand[Unit](c) {
  def text = "PASS " + password
  def handleResponse(code: Int, message: Seq[String]) = Some(if (code == 230) {
    c.state <~ State.Ready
    FutureResult(true)
  } else {
    c.state <~ State.Username
    FutureResult(false)
  })
  def name = "PASS"
}
class SystemCommand(c: Client) extends TextCommand[Unit](c) {
  def text = "SYST"
  def handleResponse(code: Int, message: Seq[String]) = Some(if (code == 215) FutureResult(true, message(0)) else FutureResult(false))
  def name = text
}
class FeaturesCommand(c: Client) extends TextCommand[Unit](c) {
  private val regex = "(.*?)(?: (.*))?".r
  def text = "FEAT"
  def handleResponse(code: Int, message: Seq[String]) = Some(if (code == 211) {
      message.drop(1).dropRight(1) foreach {
        case regex(tag, data) => tag match {
          case "EPRT" => c.features += Feature.ExtendedActive
          case "EPSV" => c.features += Feature.ExtendedPassive
          case "MDTM" => c.features += Feature.Modification
          case "MLST" => c.features += Feature.MetadataList; c.metadataList = data
          case "SIZE" => c.features += Feature.Size
          case "UTF8" => c.features += Feature.Unicode
          case _ =>
        }
        case _ =>
      }
      FutureResult(true)
    } else FutureResult(false))
  def name = text
}
class DeleteCommand(file: File, c: Client) extends TextCommand[Unit](c) {
  def text = "DELE " + file.path
  def handleResponse(code: Int, message: Seq[String]) = Some(if (code == 250) FutureResult(true) else FutureResult(false))
  def name = text
}
class QuitCommand(c: Client) extends TextCommand[Unit](c) {
  def text = "QUIT"
  def handleResponse(code: Int, message: Seq[String]) = Some(if (code == 221) {
    c.close()
    FutureResult(true)
  } else FutureResult(false))
  def name = text
}
package com.hsun324.scala.ftplite

abstract class DirectoryCommand(head: String, directory: Option[Directory], c: Client) extends TextCommand[Directory](c) {
  def text(): String = head + (directory match {
      case Some(directory) => " " + directory.path
      case _ => ""
    })
  def name = head
}
class MakeDirectoryCommand(d: Directory, c: Client) extends DirectoryCommand("MKD", Option(d), c) {
  def handleResponse(code: Int, message: Seq[String]) = Some(if (code == 257) FutureResult(true) else FutureResult(false))
}
class RemoveDirectoryCommand(d: Directory, c: Client) extends DirectoryCommand("RMD", Option(d), c) {
  def handleResponse(code: Int, message: Seq[String]) = Some(if (code == 257) FutureResult(true) else FutureResult(false))
}
private[ftplite] class DirectoryResponseCommand(h: String, d: Option[Directory], expected: Int, c: Client) extends DirectoryCommand(h, d, c) {
  val responsePattern = "\"(.*?)\".+".r
  def handleResponse(code: Int, message: Seq[String]) = Some(if (code == expected) {
      val current = if (message.length == 1) message(0) match {
        case responsePattern(dir) => Directory(dir)
        case _ => c.currentDirectory
      } else c.currentDirectory
      c.currentDirectory = current
      FutureResult(true, "", Some(current))
    } else FutureResult(false))
}
class CurrentDirectoryCommand(c: Client) extends DirectoryResponseCommand("PWD", None, 257, c);
class ChangeDirectoryCommand(d: Directory, c: Client) extends DirectoryResponseCommand("CWD", Option(d), 250, c);
class ChangeDirectoryUpCommand(c: Client) extends DirectoryResponseCommand("CDUP", None, 250, c);
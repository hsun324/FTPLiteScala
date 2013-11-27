package com.hsun324.scala.ftplite

import scala.collection.Seq
import java.util.Calendar
import java.io.InputStream
import java.nio.charset.Charset

abstract class DownloadCommand[T](c: Client) extends TextCommand[T](c) {
  protected var download: InputStream = null
  override def execute() = {
    super.execute()
    download = c.createDownload()
  }
  def handleResponse(code: Int, message: Seq[String]) = if (code == 150 || code == 125) None
    else if (code == 226) processData(Stream.continually(download.read.toByte).view.takeWhile(_ != -1)) map {x => FutureResult(true, "", Some(x))} orElse Some(FutureResult[T](false))
    else Some(FutureResult(false))
  
  def processData(data: Seq[Byte]): Option[T]
}
class RetrieveCommand(private val file: File, c: Client) extends DownloadCommand[Data](c) {
  def text = "RETR " + file.path
  def processData(data: Seq[Byte]) = Some(if (c.typeCommand.binary) new Data(data) else new Data(new String(data.toArray, ASCIICodec.charSet).replaceAll("(\n|\r)\r\n", "\r\n")))
  def name = "RETR"
}
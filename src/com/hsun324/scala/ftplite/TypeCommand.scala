package com.hsun324.scala.ftplite

import java.net.InetAddress
import java.net.ServerSocket
import java.nio.charset.Charset

abstract class TypeCommand[+T](c: Client) extends TextCommand[T](c) {
  protected object AsInt {
    def unapply(str: String) = util.Try(Some(str.toInt)) getOrElse None
  }
  def codec: io.Codec
  def handleResponse(code: Int, message: Seq[String]) = Some(if (code == 200) FutureResult(true) else FutureResult(false))
}
object ASCIICodec extends io.Codec(Charset.forName("US-ASCII"))
object BinaryCodec extends io.Codec(Charset.forName("UTF-8"))

object TypeCommand {
  trait TypeCommandFactory {
    def apply[T](c: Client): TypeCommand[T]
    def binary: Boolean
    def codec: io.Codec
  }
  
  object ASCIITypeCommandFactory extends TypeCommandFactory {
    def apply[T](c: Client) = new ASCIITypeCommand[T](c)
    def binary = false
    def codec = ASCIICodec
  }
  object BinaryTypeCommandFactory extends TypeCommandFactory {
    def apply[T](c: Client) = new BinaryTypeCommand[T](c)
    def binary = true
    def codec = BinaryCodec
  }
}
class ASCIITypeCommand[+T](c: Client) extends TypeCommand[T](c) {
  def text = "TYPE A"
  def name = "ASCII"
  def codec = ASCIICodec
}
class BinaryTypeCommand[+T](c: Client) extends TypeCommand[T](c) {
  def text = "TYPE I"
  def name = "BINARY"
  def codec = BinaryCodec
}
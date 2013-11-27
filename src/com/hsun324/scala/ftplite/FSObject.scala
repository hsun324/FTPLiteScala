package com.hsun324.scala.ftplite

import java.util.Date

abstract class FSObject(protected[ftplite] val tokens: Seq[String], protected[ftplite] val absolute: Boolean) {
  val path = (if (absolute) "/" else "") + (if (tokens.isEmpty) "" else tokens.reduceLeft(_+"/"+_))
  def :+(subpath: String): FSObject
  def ++(subpath: Seq[String]): FSObject
  def ++(subpath: File): FSObject
  def ++(subpath: Directory): FSObject
}
class FSRoot(protected[ftplite] val tokens: Seq[String]) {
  def :+(subpath: String) = new FSRoot(tokens ++ FSObject.validate(subpath.split('/')))
  def ++(subpath: Seq[String]) = new FSRoot(tokens ++ FSObject.validate(subpath));
  def file(size: Option[Int] = None, modified: Option[Date] = None) = new File(tokens, true, None, size, modified)
  def directory = new Directory(tokens, true)
}
object FSObject {
  def unapply(o: FSObject) = Option(o.path)
  private[ftplite] def validate(s: Seq[String]) = s collect {
    case s if !s.isEmpty => s.trim
  }
}

class File(t: Seq[String], a: Boolean, val data: Option[Data] = None, val size: Option[Int] = None, val modified: Option[Date] = None) extends FSObject(t, a) {
  def :+(subpath: String): File = new File(tokens ++ FSObject.validate(subpath.split('/')), a, data)
  def ++(subpath: Seq[String]): File = new File(tokens ++ FSObject.validate(subpath), a, data);
  def ++(subpath: File): File = new File(tokens ++ subpath.tokens, a, subpath.data)
  def ++(subpath: Directory): File = new File(tokens ++ subpath.tokens, a)
  def getDataSource: Option[io.Source] = data map {_.toSource}
  override def toString = "File(" + path + "," + data + ")"
}
object File {
  def apply(path: String) = new File(FSObject.validate(path.split('/')), path.startsWith("/"))
}
class Directory(t: Seq[String], a: Boolean) extends FSObject(t, a) {
  def :+(subpath: String): Directory = new Directory(tokens ++ FSObject.validate(subpath.split('/')), a)
  def ++(subpath: Seq[String]): Directory = new Directory(tokens ++ FSObject.validate(subpath), a);
  def ++(subpath: Directory): Directory = new Directory(tokens ++ subpath.tokens, a)
  def ++(subpath: File): File = new File(tokens ++ subpath.tokens, a, subpath.data)
  def root = new FSRoot(tokens)
  override def toString = "Directory(" + path + ")" 
} 
object Directory {
  val root = new Directory(Seq.empty, true)
  val current = new Directory(Seq("."), false)
  def apply(path: String) = new Directory(FSObject.validate(path.split('/')), path.startsWith("/"))
}

case class Data(val text: Option[String] = None, val data: Option[Seq[Byte]] = None) {
  def this(t: String) = this(Option(t), Option(t) map {_.getBytes(ASCIICodec.charSet)})
  def this(d: Seq[Byte]) = this(None, Option(d))
  
  if (text.isEmpty && data.isEmpty) throw new IllegalArgumentException
  def toSource = text match {
    case Some(string) => io.Source.fromString(string)
    case _ => io.Source.fromBytes(data map {_.toArray} getOrElse Array.empty)
  }
}
package com.hsun324.scala.ftplite

case class Result[+T](val message: String = "", val data: Option[T] = None) {
  def &&[U >: T] (r: Result[U]): Result[U] = Result(
      (if (!message.isEmpty) message + "\n" else "") + r.message,
      r.data orElse data.asInstanceOf[Option[U]])
}
class FutureResult[+T](val successful: Boolean, m: String = "", d: Option[T] = None) extends Result[T](m, d) {
  def &&[U >: T] (r: FutureResult[U]): FutureResult[U] = FutureResult(
      r.successful && successful,
      (if (!message.isEmpty) message + "\n" else "") + r.message,
      r.data orElse data.asInstanceOf[Option[U]])
}
object FutureResult {
  def apply[T](s: Boolean, m: String = "", d: Option[T] = None) = new FutureResult[T](s, m, d)
}
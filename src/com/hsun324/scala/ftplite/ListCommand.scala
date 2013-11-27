package com.hsun324.scala.ftplite

import java.util.Calendar

private object Split {
  def unapply(str: String) = Some(str.split("=", 2))
}
private object AsInt {
  def unapply(str: String) = util.Try(Some(str.toInt)) getOrElse None
  def apply(str: Option[String]) = util.Try(str map {_.toInt}) getOrElse None
}
private object AsDate {
  val date = "(?iu)(\\d{4})(\\d{2})(\\d{2})(\\d{2})(\\d{2})(\\d{2})".r
  def apply(d: Option[String]) = d flatMap {
    case date(AsInt(year), AsInt(month), AsInt(day), AsInt(hour), AsInt(minute), AsInt(second)) =>
      val calendar = Calendar.getInstance
      calendar.set(year, month, day, hour, minute, second);
      Some(calendar.getTime)
    case _ => None
  } 
}
private object ListGen {
  private val basicRegex = "(?iu)([-drwx]{10})\\s+(?:[^\\s]+\\s+){6,7}(.+)".r
  private val metaRegex = "(?iu)(?:(.*;) )?(.*)".r
  def proc(dir: Directory) = {
    val p: PartialFunction[String, FSObject] = {
      case metaRegex(pairs, name) =>
        val map = Option(pairs) map {x => (x.split(';') collect {
            case Split(x) if x.length > 1 => x(0) -> x(1)
          }).toMap} getOrElse Map.empty
        val q = dir.root :+ name
        if (map.getOrElse("type", "file").equals("dir")) q.directory
        else q.file(AsInt(map.get("size")), AsDate(map.get("modified")))
      case basicRegex(flag, name) => 
        val q = dir.root :+ name
        if (flag.toLowerCase.contains("d")) q.directory else q.file()
    }
    p
  }
}
class ListCommand(d: Option[Directory], c: Client) extends DownloadCommand[Seq[FSObject]](c) {
  private val dir = d map {c.currentDirectory ++ _} getOrElse c.currentDirectory

  def text = (if (c.features ? Feature.MetadataList) "MLSD" else "LIST") + " " + dir.path
  def processData(data: Seq[Byte]) = Some(new String(data.toArray, ASCIICodec.charSet).split("\r\n?|\n\r?") collect ListGen.proc(dir))
  def name = "LIST"
}
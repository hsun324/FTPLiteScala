package com.hsun324.scala.ftplite

import java.io.InputStream
import java.net.Socket
import java.util.UUID
import java.util.concurrent.LinkedBlockingQueue
import scala.actors.Actor
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.reflect.ClassTag
import java.util.concurrent.BlockingQueue
import scala.collection.JavaConversions._
import scala.util.Try
import scala.concurrent.duration.Duration
import scala.util.control.Breaks._
import scala.collection._
import java.net.ServerSocket
import scala.collection.mutable.MutableList

class Client(protected[ftplite] val host: String, protected[ftplite] val port: Int = 21) {
  protected[ftplite] val uuid = UUID.randomUUID
  protected[ftplite] val socket = new Socket(host, port)
  protected[ftplite] val state = new ClientState
  protected[ftplite] val features = new ClientFeature
  state <~ State.Username
  
  protected[ftplite] val input = socket.getInputStream
  protected[ftplite] val output = socket.getOutputStream
  
  protected[ftplite] val queue = new FIFOStream[Command[Any]]
  protected[ftplite] val reader = new ClientReader(this)
  reader.start()
  Await.ready(enqueue(new ConnectCommand(this)), Duration.Inf)
  
  protected[ftplite] var _currentDirectory = Directory.root
  def currentDirectory = state synchronized {_currentDirectory}
  protected[ftplite] def currentDirectory_=(directory: Directory) = state synchronized {_currentDirectory = directory}
  
  protected[ftplite] var _metadataList: Seq[String] = Seq.empty
  protected[ftplite] def metadataList = state synchronized {_metadataList}
  protected[ftplite] def metadataList_=(seq: Seq[String]) = state synchronized {_metadataList = seq}
  protected[ftplite] def metadataList_=(op: String) = state synchronized {_metadataList = op.split(';')}
  
  protected[ftplite] var _dataHost: String = host
  protected[ftplite] def dataHost = state synchronized {_dataHost}
  protected[ftplite] def dataHost_=(host: String) = state synchronized {_dataHost = host}
  
  protected[ftplite] var _dataPort: Int = 0
  protected[ftplite] def dataPort = state synchronized {_dataPort}
  protected[ftplite] def dataPort_=(port: Int) = state synchronized {_dataPort = port}
  
  protected[ftplite] var _modeCommand: ModeCommand.ModeCommandFactory = ModeCommand.PassiveModeCommandFactory
  protected[ftplite] def modeCommand = state synchronized {_modeCommand}
  protected[ftplite] def modeCommand_=(command: ModeCommand.ModeCommandFactory) = state synchronized {_modeCommand = command}
  
  protected[ftplite] var _typeCommand: TypeCommand.TypeCommandFactory = TypeCommand.ASCIITypeCommandFactory
  protected[ftplite] def typeCommand = state synchronized {_typeCommand}
  protected[ftplite] def typeCommand_=(typeFactory: TypeCommand.TypeCommandFactory) = state synchronized {_typeCommand = typeFactory}
  
  protected[ftplite] def enqueue[T : ClassTag](c: DownloadCommand[T]): Future[Result[T]] = enqueue(typeCommand(this), modeCommand(this), c: Command[T])
  protected[ftplite] def enqueue[T : ClassTag](c: Command[T]*): Future[Result[T]] = {
    if (state == State.Closed) throw new IllegalStateException
    if (c.length == 0) throw new IllegalArgumentException
    val command = if (c.length == 1) c(0) else new ChainedCommand(c.toIndexedSeq, this)
    queue ++ command
    future {
      (command.start !! Dependent)() match {
        case e: Exception => throw new FailureException("Failed command", FutureResult(false), e)
        case r: FutureResult[T] =>
          if (!r.successful) throw new FailureException("Failed command", r)
          r
      }
    }
  }
  protected[ftplite] def send(s: String) = {
    println(s)
    output.write((s + "\r\n").getBytes(ASCIICodec.charSet))
  }
  protected[ftplite] def close() {
    state <~ State.Closing
    socket.close()
  }
  
  protected[ftplite] def createDownload() = 
    if (modeCommand.active) new ServerSocket(dataPort).accept().getInputStream
    else new Socket(socket.getInetAddress, dataPort).getInputStream

  protected[ftplite] def createUpload() = 
    if (modeCommand.active) new ServerSocket(dataPort).accept().getOutputStream
    else new Socket(socket.getInetAddress, dataPort).getOutputStream
  
  def authenticate(user: String, password: Option[String] = None) = {
    var vector = Vector[Command[Any]]()
    vector :+= new UsernameCommand(user, this)
    if (password.isDefined) vector :+= new PasswordCommand(password.get, this)
    vector :+= new SystemCommand(this)
    vector :+= new FeaturesCommand(this)
    vector :+= new CurrentDirectoryCommand(this)
    enqueue(vector: _*)
  }
  def quit() = enqueue(new QuitCommand(this))
  
  def changeDirectory(directory: Directory) = enqueue(new ChangeDirectoryCommand(directory, this))
  def changeDirectoryUp() = enqueue(new ChangeDirectoryUpCommand(this))
  def makeDirectory(directory: Directory) = enqueue(new MakeDirectoryCommand(directory, this))
  def removeDirectory(directory: Directory) = enqueue(new RemoveDirectoryCommand(directory, this))
  
  def listDirectory(directory: Directory = null) = enqueue(new ListCommand(Option(directory), this))
  
  def setActiveMode() = modeCommand = ModeCommand.ActiveModeCommandFactory
  def setPassiveMode() = modeCommand = ModeCommand.PassiveModeCommandFactory
  
  protected[ftplite] def binary(bin: Boolean) = typeCommand = if (bin) TypeCommand.BinaryTypeCommandFactory else TypeCommand.ASCIITypeCommandFactory
  
  def retrieveFile(file: File) = {
    binary(FiletypeResolver(file))
    enqueue(new RetrieveCommand(file, this))
  }
}

object Client {
  def apply(host: String, port: Int = 21) = future {
    new Client(host, port)
  }
}

object FiletypeResolver {
  private val textTypes = mutable.Set(
    "ajx", "am", "asa", "asc", "asp", "aspx", "awk", "bat", "c", "cdf", "cf", "cfg",
    "cfm", "cgi", "cnf", "conf", "cpp", "css", "csv", "ctl", "dat", "dhtml", "diz",
    "file", "forward", "grp", "h", "hpp", "hqx", "hta", "htaccess", "htc", "htm",
    "html", "htpasswd", "htt", "htx", "in", "inc", "info", "ini", "ink", "java",
    "js", "jsp", "log", "logfile", "m3u", "m4", "m4a", "mak", "map", "model", "msg",
    "nfo", "nsi", "info", "old", "pas", "patch", "perl", "php", "php2", "php3", "php4",
    "php5", "php6", "phtml", "pix", "pl", "pm", "po", "pwd", "py", "qmail", "rb",
    "rbl", "rbw", "readme", "reg", "rss", "rtf", "ruby", "session", "setup", "sh",
    "shtm", "shtml", "sql", "ssh", "stm", "style", "svg", "tcl", "text", "threads",
    "tmpl", "tpl", "txt", "ubb", "vbs", "xhtml", "xml", "xrc", "xsl")
  def apply(file: File) = {
    val name = filename(file)
    !name.startsWith(".") && !(textTypes contains extension(name))
  }
  def +=(ext: String) = textTypes synchronized {textTypes += ext}
  def -=(ext: String) = textTypes synchronized {textTypes -= ext}
  private def filename(file: File) = file.tokens.lastOption getOrElse ""
  private def extension(file: String) = file.lastIndexOf('.') match {
      case -1 => ""
      case i => file.substring(i + 1)
    }
}

class FailureException[+T](private val message: String = "FTP Command Failed", val currentResult: Result[T], val cause: Throwable = null) extends Exception(message, cause)

class FIFOStream[A](private val queue: BlockingQueue[A] = new LinkedBlockingQueue[A]()) {
  lazy val toStream: Stream[A] = stream
  private def stream: Stream[A] = queue.take #:: stream
  def ++(as: A*) = queue addAll as
  def --(as: A*) = queue removeAll as
}

object State extends Enumeration {
  type State = Value
  val Closed, Username, Password, Ready, Read, Closing = Value
}
class ClientState {
  private var state = State.Closed
  def get = synchronized {state}
  def <~ (state: State.Value) = synchronized {this.state = state}
  def == (state: State.Value) = synchronized {this.state == state}
  def ~> = get
}

object Feature extends Enumeration {
  type Feature = Value
  val Unicode, MetadataList, Size, Modification, ExtendedPassive, ExtendedActive = Value
}
class ClientFeature {
  private val features: mutable.Set[Feature.Value] = mutable.Set[Feature.Value]()
  def += (feature: Feature.Value) = features synchronized {features += feature}
  def -= (feature: Feature.Value) = features synchronized {features -= feature}
  def ? (feature: Feature.Value) = features synchronized {features contains feature}
}

class ClientReader(private val c: Client) extends Actor {
  private val input = c.input
  private val queue = c.queue
  
  private object ToInt {
    def unapply(str: String) = Try(Option(str.toInt)) getOrElse None
  }
  def act() {
    val responseFormat = "([0-9]{3})([ -])?(.*)?".r
    val buffer = MutableList[String]()
    breakable {
      queue.toStream foreach {command =>
        command.execute()
        Stream.continually(Stream.continually(input.read.toChar).takeWhile(_ != '\n').mkString.trim) takeWhile {message => 
          println("> " + message)
          message match {
            case responseFormat(ToInt(code), delim, message) =>
              buffer += (if (message == null) "" else message)
              val q = if (delim == " " || delim == null) {
                val r = ((command !! Response(code, buffer.toList))() == false)
                buffer.clear()
                r
              } else true
              
              if (c.state == State.Closing) {
                c.state <~ State.Closed
                break
              }
              q
            case line => buffer += line; true
          }
        } foreach {_=>}
      }
    }
  }
}

case class Response (val code: Int, val message: Seq[String]);
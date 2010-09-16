package coqscala

import java.io.InputStream
import scala.actors._

class BusyStreamReader (input : InputStream) extends Runnable {
  private val bufsize = 256
  private val inbuf = new Array[Byte](bufsize)
  private var callbacks : List[Actor] = List[Actor]()

  def addActor (c : Actor) : Unit = { callbacks = c :: callbacks }
  def removeActor (c : Actor) : Unit =
    { callbacks = callbacks.filterNot(x => x == c) }

  override def run () : Unit = {
    while (true) {
      val avail = input.available
      if (avail >= 0) {
        val bytesread = input.read(inbuf, 0, if (avail == 0 | avail > bufsize) bufsize else avail)
        //Console.println("read " + bytesread + " (actors: " + callbacks.length + ")")
        if (bytesread < avail && bytesread < bufsize)
          Console.println("bytesread " + bytesread + " < avail " + avail)
        val res = new String(inbuf, 0, bytesread, "UTF-8")
        callbacks.foreach((_ : Actor) ! res)
      }
    }
  }
}

trait CoqCallback {
  def dispatch (x : CoqResponse) : Unit
}

object PrintActor extends Actor with OutputChannel[String] {
  object PCons {
    def println (x : String) : Unit = { Console.println(x) }
  }
  type Printable = { def println (x : String) : Unit }
  var stream : Printable = PCons

  private var callbacks : List[CoqCallback] = List[CoqCallback]()

  def register (c : CoqCallback) : Unit = { callbacks = c :: callbacks }

  def act() {
    while (true) {
      receive {
        case msg : String => {
          stream.println("received:" + msg)
          val coqr = ParseCoqResponse.parse(msg)
          stream.println("received (parsed):" + coqr)
          coqr.foreach(x => callbacks.foreach(_.dispatch(x)))
        }
      }
    }
  }
}

class CoqShellTokens (theorem_ : String, globalStep_ : Int, innerTheorem_ : String, localStep_ : Int) {
  val theorem : String = theorem_
  val globalStep : Int = globalStep_
  val innerTheorem : String = innerTheorem_
  val localStep : Int = localStep_
  override def toString = theorem + " " + globalStep + " " + innerTheorem + " " + localStep
}

object ValidCoqShell {
  def getTokens (s : String) : Option[CoqShellTokens] = {
    val tokens0 = s.split('\n')
    if (tokens0.length > 0) {
      val tokens1 = tokens0(tokens0.length - 1).trim.split('<')
      if (tokens1.length == 3) {
        val tokens2 = tokens1(1).split('|')
        if (tokens2.length == 3)
          Some(new CoqShellTokens(tokens1(0).trim, Integer.parseInt(tokens2(0).trim),
                                  tokens2(1).trim, Integer.parseInt(tokens2(2).trim)))
        else
          None
      } else None
    } else None
  }
}

class BoolRef {
  private var value : Boolean = false
  def getValue () : Boolean = { return value }
  def setValue (newval : Boolean) : Unit = {
    value = newval
    //Console.println("ready? " + newval)
  }
}

object ErrorOutputActor extends Actor with OutputChannel[String] {
  private var ready : BoolRef = null
  private var context : CoqShellTokens = new CoqShellTokens("", 0, "", 0)

  def setBoolRef (n : BoolRef) : Unit = { ready = n }

  def getContext () : CoqShellTokens = { context }

  def act() {
    while (true) {
      receive {
        case msg : String => ValidCoqShell.getTokens(msg) match {
          case Some(tokens : CoqShellTokens) => {
            //TODO: we could do some more ambitious checking,
            //like linearity and monotonicity of steps, but who cares?
            ready.setValue(true)
            if (context.globalStep < tokens.globalStep) //got no error
              if (context.localStep <= tokens.localStep) //and doing the same proof
                DocumentState.commit
              else
                DocumentState.undo
            context = tokens
          }
          case None => if (msg.filterNot(x => x == '\n').length > 0)
                         Console.println("couldn't parse X" + msg + "X")
        }
      }
    }
  }
}

object CoqTop {
  import java.io.OutputStream
  import java.lang.{Process,Runtime,Thread}

  private var coqout : BusyStreamReader = null
  private var coqerr : BusyStreamReader = null
  private var coqin : OutputStream = null
  private var started : Boolean = false

  private val coqtopbinary = "coqtop -emacs"
  private val ready = new BoolRef

  def getContext () : CoqShellTokens = {
    ErrorOutputActor.getContext
  }

  def isReady () : Boolean = {
    ready.getValue
  }

  def writeToCoq (data : String) : Unit = {
    if (data != null && data.length > 0) {
      if (coqin == null)
        Console.println("coqin is null")
      if (isReady) {
        val datarr = data.getBytes("UTF-8")
        coqin.write(datarr)
        //Console.println("wrote " + data)
        if (datarr(datarr.length - 1) != '\n'.toByte)
          coqin.write('\n'.toByte)
        coqin.flush
        if (datarr(datarr.length - 1) == '.'.toByte)
          //XXX: that's probably a hack which doesn't work always!
          //counterexample: foo .. \nbar.
          //TODO: find out when at the end of a sentence! (has something to do with '.')
          ready.setValue(false)
      } else {
        Console.println("please wait a bit longer")
        Thread.sleep(10)
        writeToCoq(data)
      }
    }
  }

  def startCoq () = {
    ErrorOutputActor.setBoolRef(ready)
    PrintActor.start
    ErrorOutputActor.start
    val coqproc = Runtime.getRuntime.exec(coqtopbinary)
    coqin = coqproc.getOutputStream
    coqout = new BusyStreamReader(coqproc.getInputStream)
    coqerr = new BusyStreamReader(coqproc.getErrorStream)
    coqout.addActor(PrintActor)
    coqerr.addActor(ErrorOutputActor)
    new Thread(coqout).start
    new Thread(coqerr).start
    started = true
  }
  
  def isStarted () : Boolean = {
    started
  }
}

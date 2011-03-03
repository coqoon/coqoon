/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.kopitiam

import java.io.InputStream
import scala.actors._

class BusyStreamReader (input : InputStream) extends Runnable {
  private val bufsize = 256
  private val inbuf = new Array[Byte](bufsize)
  private var callbacks : List[Actor] = List[Actor]()

  def addActor (c : Actor) : Unit = { callbacks = c :: callbacks }
  def removeActor (c : Actor) : Unit =
    { callbacks = callbacks.filterNot(_ == c) }

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

  private var callbacks : List[CoqCallback] = List[CoqCallback]() //why not actors?

  def register (c : CoqCallback) : Unit = { callbacks = c :: callbacks }

  def distribute (c : CoqResponse) : Unit = {
    callbacks.foreach(_.dispatch(c))
  }

  def act() {
    var buf = ""
    while (true) {
      receive {
        case msg : String => {
          buf = buf + msg
          if (msg.endsWith("\n")) {
            //stream.println("received:" + msg)
            val coqr = ParseCoqResponse.parse(buf)
            buf = ""
/*            coqr.foreach(x => x match {
              case CoqGoal(n, elements) =>
                Console.println("checking sanity of " + n + " in elems: " + elements)
                //sanity check: elements matching 'subgoal x is' should be equal to n
                val subg = elements.filter(x => x.startsWith("subgoal ", 1) && x.endsWith(" is:")).length
                val impls = elements.filter(x => x.endsWith("============================")).length //28 =, coq's break between assumptions and obligation
                assert(n - 1 == subg)
                assert(1 == impls)
              case y =>
            })
            */
            //stream.println("received (parsed):" + coqr)
            coqr.foreach(x => callbacks.foreach(_.dispatch(x)))
          } else Console.println("filling buffer with " + msg)
        }
      }
    }
  }
}

case class CoqShellTokens (theorem : String, globalStep : Int, context : List[String], localStep : Int) {
  override def toString = theorem + " " + globalStep + " " + context + " " + localStep
}

object ValidCoqShell {
  def getTokens (s : String) : Option[CoqShellTokens] = {
    val tokens0 = s.split('\n')
    if (tokens0.length > 0) {
      val tokens1 = tokens0(tokens0.length - 1).trim.split('<') //Thm < gstep |con|text| lstep < ?
      if (tokens1.length == 3) {
        val tokens2 = tokens1(1).split('|') //gstep |con|text| lstep
        if (tokens2.length >= 2)
          Some(new CoqShellTokens(tokens1(0).trim,
                                  Integer.parseInt(tokens2(0).trim),
                                  tokens2.drop(1).toList.dropRight(1),
                                  Integer.parseInt(tokens2.takeRight(1)(0).trim)))
        else
          None
      } else None
    } else None
  }
}

object CoqState {
  private var monotonic : Boolean = false
  private var readyforinput : Boolean = false
  private var context : CoqShellTokens = new CoqShellTokens("Coq", 0, List(), 0)

  def monoton () : Boolean = { monotonic }

  def readyForInput () : Boolean = { readyforinput }

  def shell () : CoqShellTokens = { context }
  def setShell (tokens : CoqShellTokens) : Unit = {
    val oldc = context
    context = tokens
    readyforinput = true
    if (oldc.globalStep < tokens.globalStep) //got no error
      //TODO: should actually be subset, not != in the latter comparison
      //also strictly greater in first comparison, shouldn't be it?
      if (oldc.localStep <= tokens.localStep || oldc.theorem != tokens.theorem)
        monotonic = true
    PrintActor.distribute(CoqShellReady())
  }

  def sendCommand () : Unit = { Console.println("all false now"); readyforinput = false; monotonic = false }
}

object ErrorOutputActor extends Actor with OutputChannel[String] {
  def act() {
    while (true) {
      receive {
        case msg : String => ValidCoqShell.getTokens(msg) match {
          case Some(tokens : CoqShellTokens) => {
            Console.println("set coq ready " + tokens)
            CoqState.setShell(tokens)
          }
          case None => if (msg.filterNot(_ == '\n').length > 0)
                         Console.println("couldn't parse on stderr: " + msg)
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

  def writeToCoq (data : String) : Unit = {
    if (data != null && data.length > 0) {
      if (coqin == null)
        Console.println("coqin is null")
      if (CoqState.readyForInput) {
        val datarr = data.getBytes("UTF-8")
        coqin.write(datarr)
        Console.println("wrote ..." + data.takeRight(10))
        coqin.flush
        CoqState.sendCommand
      } else {
        //Console.println("please wait a bit longer")
        Thread.sleep(10)
        writeToCoq(data)
      }
    }
  }

  def startCoq () = {
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

  def findPreviousCommand (s : String, pos : Int) : Int = {
    def beforecomment (p : Int) : Int = {
      Console.println("beforec " + p)
      val commend = s.lastIndexOf("*)", p)
      if (commend == -1)
        commend
      else {
        val commstart = s.lastIndexOf("(*", commend)
        if (commstart < s.lastIndexOf("*)", commend - 1))
          s.lastIndexOf("(*", beforecomment(commend))
        else
          commstart
      }
    }
    def finddotspace (p : Int) : Int = {
      Console.println("findd " + p)
      val dot = s.lastIndexOf(".", p)
      if (dot != -1) {
        val cend = s.lastIndexOf("*)", p)
        if (cend != -1 && cend > dot)
          finddotspace(beforecomment(p))
        else if (s(dot - 1) != '.' && (s(dot + 1) == ' ' || s(dot + 1) == '\n'))
          dot + 1
        else
          -1
      } else dot
    }
    if (pos > 2)
      finddotspace(pos - 3)
    else
      -1
  }

  def findNextCommand (s : String) : Int = {
    def aftercomment (pos : Int) : Int = {
      //Console.println("afterc called with " + pos)
      val commsta = s.indexOf("(*", pos) + 1
      if (commsta == 0)
        commsta
      else {
        val commend = s.indexOf("*)", commsta) + 1
        if (commend > s.indexOf("(*", commsta)) //nested comment
          s.indexOf("*)", aftercomment(commsta)) + 1
        else
          if (commend == 0) s.length else commend
      }
    }
    def finddotspace (pos : Int) : Int = {
      //Console.println("finddotspace called with " + pos)
      val dot = s.indexOf(".", pos)
      if (dot != -1) {
        val cstart = s.indexOf("(*", pos)
        //search whether dot is inside of comment
        if (cstart != -1 && cstart < dot)
          finddotspace(aftercomment(pos))
        else if (s(dot - 1) != '.' && (dot + 1 == s.length || s(dot + 1) == ' ' || s(dot + 1) == '\n')) //ws (EOF, SP, NL)
          dot + 2
        else //no ws
          finddotspace(dot + 1)
      } else
        -1
    }
    finddotspace(0)
  }
}

object Outputter extends CoqCallback {
  override def dispatch (x : CoqResponse) : Unit = {
    x match {
      case CoqProofCompleted() =>
      case CoqTheoremDefined(n) =>
      case CoqVariablesAssumed(n) =>
      case CoqError(m) => Console.println("error " + m)
      case x => Console.println("received " + x)
    }
  }
}

object Main extends Application {
  import java.io.{FileReader,BufferedReader}

  override def main (args : Array[String]) = {
    System.setProperty("file.encoding", "UTF-8")
    CoqTop.startCoq()
    val in = new BufferedReader(new FileReader(args(0)))
    var line : String = in.readLine()
    var buf = ""
    while (line != null) {
      buf += " " + line
      line = in.readLine()
    }
    in.close()
    Console.println("buffer is so long: " + buf.length)
    PrintActor.register(Outputter)
    var fini : Boolean = false
    while (! fini) {
      val noff = CoqTop.findNextCommand(buf)
      if (noff == -1)
        fini = true
      else {
        CoqTop.writeToCoq(buf.take(noff))
        buf = buf.drop(noff)
      }
    }
  }

}

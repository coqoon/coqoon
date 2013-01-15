/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.kopitiam

import java.io.{ InputStream, IOException }
import akka.actor.{ Actor, ActorRef }

class BusyStreamReader (input : InputStream) extends Runnable {
  private var callbacks : List[ActorRef] = List[ActorRef]()

  def addActor (c : ActorRef) : Unit = { callbacks = c :: callbacks }
  def removeActor (c : ActorRef) : Unit = { callbacks = callbacks.filterNot(_ == c) }

  override def run () : Unit = {
    try {
      while (input.available >= 0) {
        val f = input.read //blocking
        val avail = input.available
        val inbuf = new Array[Byte](avail + 1)
        //Console.println("first byte was " + f)
        inbuf(0) = f.toByte
        val bytesread = input.read(inbuf, 1, avail)
        //Console.println("read " + bytesread + " (actors: " + callbacks.length + ")")
        if (bytesread < avail) // && bytesread < bufsize)
          Console.println("bytesread " + bytesread + " < avail " + avail)
        if (bytesread > 0) {
          val res = new String(inbuf, 0, bytesread + 1, "UTF-8")
          //Console.println("distributing [" + bytesread + "]" + res + " to " + callbacks.length)
          callbacks.foreach((_ : ActorRef).tell(res))
        }
      }
    } catch {
      case e : Throwable =>
        Console.println("yay, exception! " + e)
        //nothing to do if coq dies
    }
  }
}

trait CoqCallback {
  def dispatch (x : CoqResponse) : Unit
}

object PrintActor {
  object PCons {
    def println (x : String) : Unit = { Console.println(x) }
  }
  type Printable = { def println (x : String) : Unit }
  var stream : Printable = PCons

  var callbacks : List[CoqCallback] = List[CoqCallback]() //why not actors?

  def register (c : CoqCallback) : Unit = {
    if (! callbacks.contains(c))
      callbacks = callbacks :+ c
  }

  def deregister (c : CoqCallback) : Unit = { callbacks = callbacks.filterNot(_ == c) }

  def distribute (c : CoqResponse) : Unit = {
    Console.println("distributing response to all callbacks (" + callbacks.length + "): " + c)
    callbacks.foreach(x => {
      Console.println("  distributing to " + x)
      x.dispatch(c)
    })
    Console.println(" -> done distribution")
  }
}

class PrintActorImplementation extends Actor {
  def receive = {
    case msg : String => {
      //Console.println("received message:" + msg)
      val coqr = ParseCoqResponse.parse(msg.trim)
      //Console.println("parsed response is " + coqr)
      PrintActor.distribute(coqr)
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
      val shellstring = tokens0(tokens0.length - 1).trim
      val shstring =
        if (shellstring.startsWith("<prompt>"))
          //remove <prompt> and </prompt>
          shellstring.substring(8, shellstring.length - 9)
        else
          shellstring
      val tokens1 = shstring.split('<') //Thm < gstep |con|text| lstep < ?
      if (tokens1.length == 3) {
        val tokens2 = tokens1(1).split('|') //gstep |con|text| lstep
        if (tokens2.length >= 2)
          Some(new CoqShellTokens(tokens1(0).trim,
                                  Integer.parseInt(tokens2(0).trim),
                                  tokens2.drop(1).toList.dropRight(1).toList.filterNot(_ == ""),
                                  Integer.parseInt(tokens2.takeRight(1)(0).trim)))
        else
          None
      } else None
    } else None
  }
}

object CoqState {
  private var readyforinput : Boolean = false
  private var context : CoqShellTokens = new CoqShellTokens("Coq", 0, List(), 0)

  //danger! lions! do not come here!
  def setReady () : Unit = {
    Console.println("did you handin your 27B-6?")
    readyforinput = true
    Console.println("this 27B-6 is not stamped by central services")
  }

  def readyForInput () : Boolean = { readyforinput }

  def getShell () : CoqShellTokens = context

  def setShell (tokens : CoqShellTokens) : Unit = synchronized {
    val oldc = context
    context = tokens
    //Console.println("ready!")
    readyforinput = true
    var monotonic = false
    if (oldc.globalStep < tokens.globalStep)
      //TODO: strictly greater in first, subset in the latter comparison
      if (oldc.localStep <= tokens.localStep || oldc.theorem != tokens.theorem)
        monotonic = true
    //Console.println("distributing shell ready " + monotonic + " shell " + tokens)
    PrintActor.distribute(CoqShellReady(monotonic, tokens))
    //Console.println(" -> done distributing " + tokens)
  }

  private var lastc : String = ""
  def lastCommand () : String = {
    lastc
  }

  def lastWasUndo () : Boolean = {
    lastc.startsWith("Backtrack ")
  }

  def sendCommand (c : String) : Unit = {
    //Console.println("not ready :/")
    lastc = c
    readyforinput = false
  }
}

class ErrorOutputActor extends Actor {
  def receive = {
    case msg : String =>
      //Console.println("receiving shell " + msg)
      ValidCoqShell.getTokens(msg) match {
        case Some(tokens : CoqShellTokens) => {
          //Console.println("set coq ready " + tokens)
          CoqState.setShell(tokens)
        }
        case None =>
          if (msg.filterNot(_ == '\n').length > 0) {
            Console.println("couldn't parse on stderr: " + msg)
            PrintActor.distribute(CoqUnknown(msg.trim)) //used CoqWarning here, but that doesn't work too well
          }
      }
    case x => Console.println("x here? " + x)
  }
}

object CoqTop {
  import java.io.OutputStream
  import java.lang.{Process,Runtime,Thread}

  private var coqout : BusyStreamReader = null
  private var coqerr : BusyStreamReader = null
  private var coqin : OutputStream = null
  private var started : Boolean = false
  private var coqprocess : Process = null
  var coqpath : String = ""
  var pid : String = ""

  val coqtopbinary = "coqtop"
  private val coqarguments = "-emacs"
  private var waiting : Int = 0

  def computeCommentOffset (x : String, off : Int) : Int = {
    val st = x.indexOf("(*")
    val end = x.indexOf("*)")
    var rst : Int = st
    var isend : Boolean = false
    while (rst < end && isend == false) {
      val ni = x.indexOf("(*", rst + 1)
      if (ni != -1 && ni < end) rst = ni
      else isend = true
    }
    if (st == -1 && end == -1)
      0
    else
      if (rst >= off)
        0
      else
        end + 2 - rst + computeCommentOffset(x.substring(0, rst) + x.substring(end + 2), off)
  }

  def filterComments (x : String) : String = {
    val st = x.indexOf("(*")
    val end = x.indexOf("*)")
    var rst : Int = st
    var isend : Boolean = false
    while (rst < end && isend == false) {
      val ni = x.indexOf("(*", rst + 1)
      if (ni != -1 && ni < end) rst = ni
      else isend = true
    }
    if (st == -1 && end == -1)
      x
    else
      filterComments(x.substring(0, rst) + x.substring(end + 2))
  }

  def writeToCoq (dat : String) : Unit = {
    val data = filterComments(dat)
    if (started && data != null && data.length > 0) {
      if (coqin == null)
        Console.println("coqin is null")
      //Console.println("ready? " + CoqState.readyForInput + ", sending " + data.take(10) + "..." + data.takeRight(10))
      if (CoqState.readyForInput) {
        CoqState.sendCommand(data)
        val datarr = data.getBytes("UTF-8")
        coqin.write(datarr)
        if (data.length < 20)
          Console.println("wrote " + data)
        else
          Console.println("wrote " + data.take(10) + "..." + data.takeRight(10))
        if (data.endsWith(".")) //we've EOF or command, need whitespace
          coqin.write("\n".getBytes("UTF-8"))
        coqin.flush
      } else if (waiting > 400) {
        Console.println("initiating self-destruct mechanism")
        killCoq
      } else {
        Console.println("please wait a bit longer")
        Thread.sleep(50)
        waiting += 1
        writeToCoq(data)
      }
    }
  }

  var printactor : ActorRef = null
  var erroroutputactor : ActorRef = null

  import akka.actor.ActorSystem
  import akka.actor.Props
  def init () : Unit = {
    val system = ActorSystem("Kopitiam")
    printactor = system.actorOf(Props[PrintActorImplementation], name = "PrintActor")
    erroroutputactor = system.actorOf(Props[ErrorOutputActor], name = "ErrorOutputActor")
  }

  def killCoq () : Unit = {
    started = false
    if (coqprocess != null)
      coqprocess.destroy
    coqprocess = null
    CoqState.setReady
  }

  def interruptCoq () : Unit = {
    if (coqprocess != null) {
      if (isWin)
        Console.println("sorry, nothing to see here")
      else {
        Console.println("sending ctrl-c to coq (pid: " + pid + ")")
        Runtime.getRuntime.exec("/bin/kill -INT " + pid)
      }
    }
  }

  def isWin () : Boolean = {
    System.getProperty("os.name").toLowerCase.indexOf("windows") != -1
  }


  import java.io.File

  def checkForCoqBinary () : Boolean = {
    val end = if (isWin) ".exe" else ""
    return (new File(coqpath + coqtopbinary + end).exists)
  }

  def startCoq () : Boolean = {
    val end = if (isWin) ".exe" else ""
    if (!checkForCoqBinary) {
      Console.println("can't find coqtop binary (in: " + coqpath + coqtopbinary + end + ")")
      return false
    }
    if (isWin)
      coqprocess = Runtime.getRuntime.exec(coqpath + coqtopbinary + end + " " + coqarguments)
    else {
      //due to the lack of Java's possibility to send signals to processes,
      //we start coqtop in a shell (and thus can send ctrl+c sequences manually)
      ///bin/sh -c only works on unix; but happily there's cmd.exe /c on windows
      val exec =
        if (! new File("/bin/sh").exists) {
          Console.println("/bin/sh not found")
          List("")
        } else
          List("/bin/sh", "-c")
      if (exec.length == 1)
        return false
      val strarr = (exec ++ List("echo $$; exec " + coqpath + coqtopbinary + " " + coqarguments)).toArray
      //Console.println("executing:" + strarr.toList)
      coqprocess = Runtime.getRuntime.exec(strarr)
      val cout = coqprocess.getInputStream
      val pidarray = new Array[Byte](80) //should be enough for pid
      cout.read(pidarray, 0, 80)
      pid = new String(pidarray)
    }
    coqin = coqprocess.getOutputStream
    coqout = new BusyStreamReader(coqprocess.getInputStream)
    coqerr = new BusyStreamReader(coqprocess.getErrorStream)
    coqout.addActor(printactor)
    coqerr.addActor(erroroutputactor)
    waiting = 0
    started = true
    new Thread(coqout).start
    new Thread(coqerr).start
    true
  }

  def isStarted () : Boolean = { started }

  def findPreviousCommand (s : String, start : Int) : Int = {
    if (start == 0) 0
    else {
      var cdepth : Int = 0
      var i : Int = start
      var found : Boolean = false
      while (i > 0 && ! found) {
        val c = s(i)
        if (c == ')' && s(i - 1) == '*')
          cdepth += 1
        else if (c == '*' && s(i - 1) == '(' && cdepth > 0)
          cdepth -= 1
        else if (cdepth == 0 && c == '.' && (s(i + 1) == '\n' || s(i + 1) == ' ' || s(i + 1) == '\r' || s(i + 1) == '\t') && s(i - 1) != '.')
          found = true
        if (! found)
          i -= 1
      }
      if (found)
        i
      else
        0
    }
  }

  def findNextCommand (s : String) : Int = {
    if (s == "") -1
    else {
      var cdepth : Int = 0
      var i : Int = 0
      var found : Boolean = false
      while (i < s.length && ! found) {
        val c = s(i)
        if (c == '(' && s(i + 1) == '*')
          cdepth += 1
        else if (c == '*' && s(i + 1) == ')' && cdepth > 0)
          cdepth -= 1
        else if (cdepth == 0 && c == '.' && (i + 1 == s.length || s(i + 1) == '\n' || s(i + 1) == ' ' || (s(i + 1) == '\r' && s(i + 2) == '\n') || s(i + 1) == '\t') && (i == 0 || s(i - 1) != '.'))
          found = true
        i += 1
      }
      //Console.println("find next returns " + i)
      if (found)
        i
      else
        -1
    }
  }
}

object Outputter extends CoqCallback {
  override def dispatch (x : CoqResponse) : Unit = {
    x match {
      case CoqProofCompleted() =>
      case CoqTheoremDefined(n) =>
      case CoqVariablesAssumed(n) =>
      case CoqError(m, n, s, l) => Console.println("error " + m)
      case x => //Console.println("received " + x)
    }
  }
}

object Main extends App {
  import java.io.{File,FileInputStream,InputStreamReader,BufferedReader}

  override def main (args : Array[String]) = {
    System.setProperty("file.encoding", "UTF-8")
    CoqTop.init()
    CoqTop.startCoq()
    val in = new BufferedReader(new InputStreamReader(new FileInputStream(new File(args(0))), "UTF-8"))
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
      if (noff == -1) {
        Console.println("no next command")
        fini = true
      } else {
        CoqTop.writeToCoq(buf.take(noff))
        buf = buf.drop(noff)
      }
    }
  }

}

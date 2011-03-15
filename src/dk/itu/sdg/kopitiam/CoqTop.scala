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
    while (input.available >= 0) {
      val avail = input.available
      val bytesread = input.read(inbuf, 0, if (avail == 0 | avail > bufsize) bufsize else avail)
      //Console.println("read " + bytesread + " (actors: " + callbacks.length + ")")
      if (bytesread < avail && bytesread < bufsize)
        Console.println("bytesread " + bytesread + " < avail " + avail)
      if (bytesread > 0) {
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

  var callbacks : List[CoqCallback] = List[CoqCallback]() //why not actors?

  def register (c : CoqCallback) : Unit = { callbacks = c :: callbacks }
  def deregister (c : CoqCallback) : Unit = { callbacks = callbacks.filterNot(_ == c) }

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
            stream.println("received message:" + msg)
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
          } //else Console.println("filling buffer with " + msg)
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
  }

  def sendCommand () : Unit = {
    //Console.println("not ready :/")
    readyforinput = false
  }
}

object ErrorOutputActor extends Actor with OutputChannel[String] {
  def act() {
    while (true) {
      receive {
        case msg : String =>
          Console.println("receiving shell " + msg)
          ValidCoqShell.getTokens(msg) match {
            case Some(tokens : CoqShellTokens) => {
              Console.println("set coq ready " + tokens)
              CoqState.setShell(tokens)
            }
            case None =>
              if (msg.filterNot(_ == '\n').length > 0)
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
  private var coqprocess : Process = null
  var coqpath : String = ""
  var pid : String = ""

  private val coqtopbinary = "coqtop"
  private val coqarguments = "-emacs"

  def writeToCoq (data : String) : Unit = {
    if (started && data != null && data.length > 0) {
      if (coqin == null)
        Console.println("coqin is null")
      //Console.println("ready? " + CoqState.readyForInput + ", sending " + data.take(10) + "..." + data.takeRight(10))
      if (CoqState.readyForInput) {
        val datarr = data.getBytes("UTF-8")
        coqin.write(datarr)
        if (data.length < 20)
          Console.println("wrote " + data)
        else
          Console.println("wrote " + data.take(10) + "..." + data.takeRight(10))
        if (data.endsWith(".")) //we've EOF or command, need whitespace
          coqin.write("\n".getBytes("UTF-8"))
        coqin.flush
        CoqState.sendCommand
      } else {
        Console.println("please wait a bit longer")
        Thread.sleep(10)
        writeToCoq(data)
      }
    }
  }

  def init () : Unit = {
    PrintActor.start
    ErrorOutputActor.start
  }

  def killCoq () : Unit = {
    started = false
    if (coqprocess != null)
      coqprocess.destroy
    coqprocess = null
  }

  def interruptCoq () : Unit = {
    if (coqprocess != null) {
      Console.println("sending ctrl-c to coq (pid: " + pid + ")")
      Runtime.getRuntime.exec("/bin/kill -INT " + pid)
    }
  }

  import java.io.File
  def startCoq () : Boolean = {
    if (! new File(coqpath + coqtopbinary).exists) {
      Console.println("can't find coqtop binary (in: " + coqpath + coqtopbinary + ")")
      false
    } else {
      //due to the lack of Java's possibility to send signals to processes,
      //we start coqtop in a shell (and thus can send ctrl+c sequences manually)
      ///bin/sh -c only works on unix; but happily there's cmd.exe /c on windows
      val exec =
        if (System.getProperty("os.name").toLowerCase.indexOf("windows") != -1)
          //if (! new File("c:\\windows\\system32\\cmd.exe").exists) {
          //  Console.println("cmd.exe not found")
            List("")
          //} else
          //  List("c:\\windows\\system32\\cmd.exe", "/c")
        else //UNIX
          if (! new File("/bin/sh").exists) {
            Console.println("/bin/sh not found")
            List("")
          } else
            List("/bin/sh", "-c")
      if (exec.length == 1)
        false
      else {
        val strarr = (exec ++ List("echo $$; exec " + coqpath + coqtopbinary + " " + coqarguments)).toArray
        //Console.println("executing:" + strarr.toList)
        coqprocess = Runtime.getRuntime.exec(strarr)
        val cout = coqprocess.getInputStream
        val pidarray = new Array[Byte](80) //should be enough for pid
        cout.read(pidarray, 0, 80)
        pid = new String(pidarray)
        coqin = coqprocess.getOutputStream
        coqout = new BusyStreamReader(coqprocess.getInputStream)
        coqerr = new BusyStreamReader(coqprocess.getErrorStream)
        coqout.addActor(PrintActor)
        coqerr.addActor(ErrorOutputActor)
        new Thread(coqout).start
        new Thread(coqerr).start
        started = true
        true
      }
    }
  }
  
  def isStarted () : Boolean = {
    started
  }

  def findPreviousCommand (s : String, pos : Int) : Int = {
    def beforecomment (p : Int) : Int = {
      //Console.println("beforec " + p)
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
      //Console.println("findd " + p)
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
    if (s == "") -1
    else {
      var cdepth : Int = 0
      var i : Int = 0
      var found : Boolean = false
      while (i < s.length && ! found) {
        val c = s(i)
        if (c == '(' && s(i + 1) == '*')
          cdepth += 1
        else if (c == '*' && s(i + 1) == ')')
          cdepth -= 1
        else if (cdepth == 0 && c == '.' && (i + 1 == s.length || s(i + 1) == '\n' || s(i + 1) == ' ') && (i == 0 || s(i - 1) != '.'))
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
      case CoqError(m) => Console.println("error " + m)
      case x => //Console.println("received " + x)
    }
  }
}

object Main extends Application {
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

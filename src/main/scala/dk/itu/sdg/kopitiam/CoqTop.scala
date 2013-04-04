/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.kopitiam

case class CoqShellTokens (theorem : String, globalStep : Int, context : List[String], localStep : Int) {
  override def toString = theorem + " " + globalStep + " " + context + " " + localStep
}

object CoqTop {
  def computeCommentOffset (x : String, off : Int) : Int = {
    val st = x.indexOf("(*")
    val end = x.indexOf("*)")
    //Console.println("commentoff: " + st + " till " + end)
    var rst : Int = st
    var isend : Boolean = false
    while (rst < end && isend == false) {
      val ni = x.indexOf("(*", rst + 1)
      if (ni != -1 && ni < end)
        rst = ni
      else
        isend = true
      //Console.println("rst " + rst + " isend " + isend)
    }
    //Console.println("finished: st " + st + " end " + end + " rst " + rst + " off " + off)
    if (st == -1 && end == -1)
      0
    else
      if (rst == off)
        end
      else if (rst > off)
        0
      else {
        //Console.println("recursively calling myself")
        end + 2 - rst + computeCommentOffset(x.substring(0, rst) + x.substring(end + 2), off)
      }
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

  def writeToCoq (dat : String) : Unit = ()

  val dummy = new CoqShellTokens("Coq", 0, List(), 0)

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

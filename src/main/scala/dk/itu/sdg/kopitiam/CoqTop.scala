/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.kopitiam

case class CoqShellTokens (theorem : String, globalStep : Int, context : List[String], localStep : Int) {
  override def toString = theorem + " " + globalStep + " " + context + " " + localStep
}

object CoqTop {
  val dummy = new CoqShellTokens("Coq", 0, List(), 0)

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

package dk.itu.sdg.util

import java.io._

object IO {
  
  /*
    Returns the contents of a file as a String
  */
  def readContentsOfFile(file : File) : String = {
    val reader = new BufferedReader(new FileReader(file))
    var text = "" // TODO: Use string builder for better formance
    var line = reader.readLine()
    while (line != null) {
      text = text + " " + line
      line = reader.readLine()
    }
    text
  }
  
}
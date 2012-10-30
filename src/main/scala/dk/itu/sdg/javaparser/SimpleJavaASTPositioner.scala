/* (c) 2012 HAnnes Mehnert */

package dk.itu.sdg.javaparser

object SimpleJavaASTPositioner {

  import org.eclipse.jface.text.IDocument

  def annotatePositions (xs : List[SJDefinition], doc : IDocument) : Unit = {
    xs.foreach(annotatePosition(_, doc))
  }

  def annotatePosition (d : SJDefinition, doc : IDocument) : Unit = {
    d match {
      case x : SJClassDefinition =>
        x.body.foreach(annotateBody(_, doc))
    }
  }

  def annotateBody (d : SJBodyDefinition, doc : IDocument) : Unit = {
    d match {
      case x : SJMethodDefinition =>
        val spos = doc.getLineOffset(x.pos.line - 1) + x.pos.column - 1
        val epos = annotateStatements(x.body, doc, spos)
        x.setJavaPos(spos, epos - spos)
      case _ =>
    }
  }

  import scala.collection.immutable.Stack
  def annotateStatements (d : List[SJStatement], doc : IDocument, off : Int) : Int = {
    var last : Option[SJStatement] = None
    var todo : Stack[SJStatement] = new Stack[SJStatement]()
    var oldstart : Int = 0
    todo = todo.pushAll(d.reverse)
    while (! todo.isEmpty) {
      val x = todo.top
      todo = todo.pop
      x match {
        case SJConditional(t, c, a) =>
          todo = todo.pushAll(a.reverse)
          todo = todo.pushAll(c.reverse)
        case SJWhile(t, b) =>
          todo = todo.pushAll(b.reverse)
        case x =>
          val start = doc.getLineOffset(x.pos.line - 1) + x.pos.column - 1
          last match {
            case None => //first
            case Some(st) => st.setJavaPos(oldstart, start - oldstart)
          }
          last = Some(x)
          oldstart = start
      }
    }
    //last element: how to compute its length? -- search for closing curly
    val start = doc.get.indexOf("}", oldstart)
    last match {
      case None => //first
      case Some(st) => st.setJavaPos(oldstart, start - oldstart)
    }
    start
  }
}

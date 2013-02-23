/* (c) 2013 Hannes Mehnert */

package dk.itu.sdg.kopitiam

trait JavaToCoreJava extends VisitingAST {
  import org.eclipse.jface.text.IDocument
  import org.eclipse.jdt.core.dom.ASTNode
  def translateAST (root : ASTNode, doc : IDocument) : Unit = {
    val co = new Translator(doc)
    root.accept(co)
  }

  import org.eclipse.jdt.core.dom.{QualifiedName}
  class Translator (doc : IDocument) extends Visitor {
    override def visitNode (node : ASTNode) : Boolean = {
      node match {
        case x : QualifiedName =>
          if (x.getQualifier.isInstanceOf[QualifiedName])
            //uh oh, problem!
            Console.println("PROBLEM")
        case x =>
      }
      true
    }

    override def endVisitNode (node : ASTNode) : Unit = { }

  }
}


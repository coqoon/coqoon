/* (c) 2012 Hannes Mehnert */

package dk.itu.sdg.kopitiam.javap

import dk.itu.coqoon.core.utilities.TryCast

object EclipseJavaASTProperties {
  import org.eclipse.jdt.core.dom._
  private val coqDefinition : String = "dk.itu.sdg.kopitiam.coqDefinition"
  def getDefinition(a : ASTNode) : Option[List[String]] =
    TryCast[List[String]](a.getProperty(coqDefinition))
  def setDefinition(a : ASTNode, p : Option[List[String]]) =
    a.setProperty(coqDefinition, p.orNull)

  private val coqSpecification : String = "dk.itu.sdg.kopitiam.coqSpecification"
  def getSpecification(a : ASTNode) : Option[List[String]] =
    TryCast[List[String]](a.getProperty(coqSpecification))
  def setSpecification(a : ASTNode, p : Option[List[String]]) =
    a.setProperty(coqSpecification, p.orNull)

  private val coqProof : String = "dk.itu.sdg.kopitiam.coqProof"
  def getProof(a : ASTNode) : Option[List[String]] =
    TryCast[List[String]](a.getProperty(coqProof))
  def setProof(a : ASTNode, p : Option[List[String]]) =
    a.setProperty(coqProof, p.orNull)

  private val coqEnd : String = "dk.itu.sdg.kopitiam.coqEnd"
  def getEnd(a : ASTNode) : Option[String] =
    TryCast[String](a.getProperty(coqEnd))
  def setEnd(a : ASTNode, p : Option[String]) =
    a.setProperty(coqEnd, p.orNull)

  private val method : String = "dk.itu.sdg.kopitiam.method"
  def getMethod(a : ASTNode) : Option[MethodDeclaration] =
    TryCast[MethodDeclaration](a.getProperty(method))
  def setMethod(a : ASTNode, p : Option[MethodDeclaration]) =
    a.setProperty(method, p.orNull)

  private val precondition : String = "dk.itu.sdg.kopitiam.precondition"
  def getPrecondition(a : ASTNode) : Option[Initializer] =
    TryCast[Initializer](a.getProperty(precondition))
  def setPrecondition(a : ASTNode, p : Option[Initializer]) =
    a.setProperty(precondition, p.orNull)

  private val postcondition : String = "dk.itu.sdg.kopitiam.postcondition"
  def getPostcondition(a : ASTNode) : Option[Initializer] =
    TryCast[Initializer](a.getProperty(postcondition))
  def setPostcondition(a : ASTNode, p : Option[Initializer]) =
    a.setProperty(postcondition, p.orNull)

  private val quantification = "dk.itu.sdg.kopitiam.quantification"
  def getQuantification(a : ASTNode) : Option[Initializer] =
    TryCast[Initializer](a.getProperty(quantification))
  def setQuantification(a : ASTNode, p : Option[Initializer]) =
    a.setProperty(quantification, p.orNull)

  private val antiquoteContent = "dk.itu.sdg.kopitiam.contentExpr"
  def getAntiquoteContent(a : Statement) : Option[String] =
    TryCast[String](a.getProperty(antiquoteContent))

  val coqOffset : String = "dk.itu.sdg.kopitiam.coqOffset"
  val specOffset : String = "dk.itu.sdg.kopitiam.specOffset"
}

object EclipseJavaHelper {
  import scala.collection.JavaConversions.asScalaBuffer

  import org.eclipse.jdt.core.ITypeRoot
  import org.eclipse.jdt.ui.JavaUI
  import org.eclipse.ui.IEditorInput
  def getRoot (ei : IEditorInput) : ITypeRoot =
    TryCast[ITypeRoot](JavaUI.getEditorInputJavaElement(ei)).orNull

  import org.eclipse.jdt.core.dom.{CompilationUnit, ASTParser, AST}
  import org.eclipse.jdt.core.{ICompilationUnit, IClassFile}
  import org.eclipse.jdt.ui.SharedASTProvider
  def getCompilationUnit (input : ITypeRoot) : CompilationUnit =
      TryCast[ICompilationUnit](input) match {
    case Some(cu) =>
      /* XXX: Is this cache actually safe to use here? */
      SharedASTProvider.getAST(cu, SharedASTProvider.WAIT_YES, null)
    case None =>
      val parser : ASTParser = ASTParser.newParser(AST.JLS4)
      parser.setResolveBindings(true)
      parser.setSource(input.asInstanceOf[IClassFile])
      parser.setStatementsRecovery(true)
      parser.setBindingsRecovery(true)
      parser.setIgnoreMethodBodies(false)
      parser.createAST(null).asInstanceOf[CompilationUnit]
  }

  import org.eclipse.jdt.core.dom.{ASTNode, MethodDeclaration, Initializer}
  def findMethod (x : ASTNode) : Option[MethodDeclaration] = Option(x) match {
    case None => None
    case Some(y : MethodDeclaration) => Some(y)
    case Some(y : Initializer) => EclipseJavaASTProperties.getMethod(y)
    case Some(y) => findMethod(y.getParent)
  }

  def findASTNode (root : ASTNode, offset : Int, length : Int) : ASTNode = {
    val nf = new NodeFinder(offset, length)
    root.accept(nf)
    val result = nf.coveredNode
    val sr : ASTNode = nf.coveringNode.getOrElse(null)
    result match {
      case Some(x) =>
        if (x.getStartPosition != offset || x.getLength != length)
          sr
        else
          x
      case None => sr
    }
  }

  class NodeFinder (off : Int, len : Int) extends VisitingAST.Visitor {
    import org.eclipse.jdt.core.dom.Statement
    var coveringNode : Option[ASTNode] = None
    var coveredNode : Option[ASTNode] = None
    override def visitNode (node : ASTNode) : Boolean = {
      val f = (node : ASTNode) => {
        val ns = node.getStartPosition
        val ne = ns + node.getLength
        if (ne < off || off + len < ns)
          false
        else if (ns <= off && off + len <= ne)
          coveringNode = Some(node)
        if (off <= ns && ne <= off + len) {
          if (coveringNode == Some(node)) {
            coveredNode = Some(node)
            true
          } else if (coveredNode == None)
            coveredNode = Some(node)
          false
        } else
          true
      }
      node match {
        case node : Statement => f(node)
        case md : MethodDeclaration => f(md)
        case i : Initializer => f(i)
        case x =>
          //Console.println("visitNode: not using " + x)
          true
      }
    }
  }

  import org.eclipse.jface.text.IDocument
  def walkAST (jes : JavaEditorState, root : ASTNode, doc : IDocument) : Boolean = {
    val co = new CoqOutput(jes, doc)
    root.accept(co)
    co.getSuccess
  }
}

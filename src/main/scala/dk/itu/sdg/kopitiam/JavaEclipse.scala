/* (c) 2012 Hannes Mehnert */

package dk.itu.sdg.kopitiam

object EclipseJavaASTProperties {
  val coqDefinition : String = "dk.itu.sdg.kopitiam.coqDefinition"
  val coqSpecification : String = "dk.itu.sdg.kopitiam.coqSpecification"
  val coqProof : String = "dk.itu.sdg.kopitiam.coqProof"
  val coqEnd : String = "dk.itu.sdg.kopitiam.coqEnd"
  val coqShell : String = "dk.itu.sdg.kopitiam.coqShell"
  val method : String = "dk.itu.sdg.kopitiam.method"
  val precondition : String = "dk.itu.sdg.kopitiam.precondition"
  val postcondition : String = "dk.itu.sdg.kopitiam.postcondition"
  val quantification : String = "dk.itu.sdg.kopitiam.quantification"
  val coqOffset : String = "dk.itu.sdg.kopitiam.coqOffset"
  val specOffset : String = "dk.itu.sdg.kopitiam.specOffset"
}

trait EclipseJavaHelper extends VisitingAST {
  import org.eclipse.jdt.core.ITypeRoot
  import org.eclipse.jdt.ui.JavaUI
  import org.eclipse.ui.IEditorInput
  def getRoot (ei : IEditorInput) : ITypeRoot = {
    val input = JavaUI.getEditorInputJavaElement(ei)
    if (input.isInstanceOf[ITypeRoot])
      input.asInstanceOf[ITypeRoot]
    else {
      Console.println("something bad happened, got " + input)
      null
    }
  }

  import org.eclipse.jdt.core.dom.{CompilationUnit, ASTParser, AST}
  import org.eclipse.jdt.core.{ICompilationUnit, IClassFile}
  import org.eclipse.jdt.ui.SharedASTProvider
  def getCompilationUnit (input : ITypeRoot) : CompilationUnit = {
    var root : CompilationUnit = null
    //if we have a cache!
    if (input.isInstanceOf[ICompilationUnit]) {
      val cu : ICompilationUnit = input.asInstanceOf[ICompilationUnit]
      root = SharedASTProvider.getAST(cu, SharedASTProvider.WAIT_YES, null)
    } else {
      val parser : ASTParser = ASTParser.newParser(AST.JLS4)
      parser.setResolveBindings(true)
      parser.setSource(input.asInstanceOf[IClassFile])
      parser.setStatementsRecovery(true)
      parser.setBindingsRecovery(true)
      parser.setIgnoreMethodBodies(false)
      root = parser.createAST(null).asInstanceOf[CompilationUnit]
    }
    //Console.println("root (which we return) is " + root.getClass.toString)
    return root
  }

  import org.eclipse.jdt.core.dom.{ASTNode, MethodDeclaration, Initializer}
  def findMethod (x : ASTNode) : Option[MethodDeclaration] = {
    if (x == null)
      None
    else {
      x match {
        case y : MethodDeclaration => Some(y)
        case y : Initializer =>
          val m = y.getProperty(EclipseJavaASTProperties.method)
          if (m != null) Some(m.asInstanceOf[MethodDeclaration]) else None
        case y => findMethod(y.getParent)
      }
    }
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

  class NodeFinder (off : Int, len : Int) extends Visitor {
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
          if (coveringNode == node) {
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
  def walkAST (root : ASTNode, doc : IDocument) : Unit = {
    val co = new CoqOutput(doc)
    root.accept(co)
  }

  class CoqOutput (doc : IDocument) extends Visitor {
    import scala.collection.immutable.Stack
    var offset : Int = 0;
    var specs : List[Initializer] = List[Initializer]()

    //method local
    var deps : Set[String] = Set[String]()
    var ret : String = "`0"

    private def extractString (x : Pair[Initializer, String]) : String = {
      //Console.println("string is " + x._2)
      val idx = x._2.indexOf(":")
      val r = x._2.substring(idx + 1, x._2.length - 2).trim
      //Console.println("r is " + r)
      r
    }

    import org.eclipse.jdt.core.dom.{Modifier, PrimitiveType, SingleVariableDeclaration}
    override def visitNode (node : ASTNode) : Boolean = {
      node match {
        case x : Initializer =>
          specs = x :: specs
        case x : MethodDeclaration =>
          ret = "`0"
          deps = Set[String]()
          //Console.println("got a method declaration. now what? specs: " + specs.size)
          val body = x.getBody
          val name = x.getName.getIdentifier
          val st = (x.getModifiers & Modifier.STATIC) == Modifier.STATIC
          val arguments = scala.collection.JavaConversions.asBuffer(x.parameters).map(_.asInstanceOf[SingleVariableDeclaration]).toList.map(_.getName)
          val argli = arguments.map(printE(_))
          val arglis = if (st) argli else "\"this\"" :: argli
          val arglist = arglis.mkString("[", ";", "]")
          if (body != null) {
            val bd = getBodyString(body)
            bd match {
              case Some(y) =>
                val id = name + "M"
                val defs = "Definition " + name + "_body := " + y + """.
Definition """ + id + " := Build_Method " + arglist + " " + name + "_body " + ret + "."
                x.setProperty(EclipseJavaASTProperties.coqDefinition, defs)
              case _ =>
            }
          }

          var quant : Option[Pair[Initializer, String]] = None;
          var pre : Option[Pair[Initializer, String]] = None;
          var post : Option[Pair[Initializer, String]] = None;

          for (s <- specs) {
            //adjust pointers!
            s.setProperty(EclipseJavaASTProperties.method, x)
            val spectxt = doc.get(s.getStartPosition, s.getLength)
            val lvaridx = spectxt.indexOf("lvars:")
            if (lvaridx > -1) {
              if (quant != None)
                reportError("multiple logical variable statements, only one supported", s)
              x.setProperty(EclipseJavaASTProperties.quantification, s)
              quant = Some((s, spectxt))
            } else {
              val preidx = scala.math.max(spectxt.indexOf("precondition:"), spectxt.indexOf("requires:"))
              if (preidx > -1) {
                if (pre != None)
                  reportError("multiple precondition statements, only one supported", s)
                x.setProperty(EclipseJavaASTProperties.precondition, s)
                pre = Some((s, spectxt))
              } else {
                val postidx = scala.math.max(spectxt.indexOf("postcondition:"), spectxt.indexOf("ensures:"))
                if (postidx > -1) {
                  if (post != None)
                    reportError("multiple postcondition statements, only one supported", s)
                  x.setProperty(EclipseJavaASTProperties.postcondition, s)
                  post = Some((s, spectxt))
                }
              }
            }
          }
          var spec1 = "Definition " + name + """_spec :=
  ("""

          val qua =
            quant match {
              case None => ""
              case Some(x) =>
                x._1.setProperty(EclipseJavaASTProperties.coqOffset, spec1.length)
                val q1 = extractString(x).split(",")
                val q2 = q1.mkString("[A] ", ", [A]", "")
                q2 + ", "
            }

          val clazz = printE(findClass(x).getName)
          val spec2 = spec1 + qua + clazz + " :.: \"" + name + "\" |-> " + arglist + """
  {{ """

          val rtype = x.getReturnType2
          val rets =
            if (rtype != null && rtype.isInstanceOf[PrimitiveType] && rtype.asInstanceOf[PrimitiveType].getPrimitiveTypeCode == PrimitiveType.VOID)
              "\"\", "
            else
              ""

          val pr =
            pre match {
              case None => reportError("no precondition provided", x); ""
              case Some(x) =>
                x._1.setProperty(EclipseJavaASTProperties.coqOffset, spec2.length)
                extractString(x)
            }

          val spec3 = spec2 + pr + " }}-{{ " + rets

          val po =
            post match {
              case None => reportError("no postcondition provided", x); ""
              case Some(x) =>
                x._1.setProperty(EclipseJavaASTProperties.coqOffset, spec3.length)
                extractString(x)
            }

          val spec = spec3 + po + " }})."
          //set offsets for quant._1, pre._1, post._1

          //Console.println("spec: " + spec)
          x.setProperty(EclipseJavaASTProperties.coqSpecification, spec)
          specs = List[Initializer]()


          val rdep = deps.map(_ + "_spec")
          val rdeps =
            if (rdep.size == 0) ""
            else if (rdep.size == 1) "|> " + rdep.last
            else
              "|> " + rdep.mkString("(", "[/\\]", ")")
          val suff = if (deps.contains(name)) " at 2 " else ""
          val prfhead = "Lemma valid_" + name + "_" + clazz.drop(1).dropRight(1) + ": " + rdeps + " |= " + name + """_spec.
Proof.
  unfold """ + name + "_spec" + suff + "; unfold_spec.\n"
          //Console.println("proof is " + prfhead)
          x.setProperty(EclipseJavaASTProperties.coqProof, prfhead)

        case x => //Console.println("found here: " + x.getClass.toString)
      }
      true
    }

    import scala.collection.immutable.Stack
    import org.eclipse.jdt.core.dom.{AbstractTypeDeclaration, TypeDeclaration, VariableDeclarationFragment}
    override def endVisitNode (node : ASTNode) : Unit =
      node match {
        case x : TypeDeclaration =>
          var methods : List[String] = List[String]()
          for (m <- x.getMethods) {
            val nam = m.getName.getIdentifier
            val id = nam + "M"
            methods ::= "\"" + nam + "\" " + id
          }
          //class def (Build_Class) - fields + methods
          val nam = x.getName.getIdentifier
          val fieldnames = x.getFields.map(x => scala.collection.JavaConversions.asBuffer(x.fragments).map(_.asInstanceOf[VariableDeclarationFragment]).toList.map(_.getName.getIdentifier)).flatten
          val fields = fieldnames.foldRight("(SS.empty)")("(SS.add \"" + _ + "\" " + _ + ")")
          val metstring = methods.foldRight("(SM.empty _)")("(SM.add " + _ + " " + _ + ")")
          val cd = "Definition " + nam + " := Build_Class " + fields + " " + metstring + "."

          //Console.println(cd)
          x.setProperty(EclipseJavaASTProperties.coqDefinition, cd)
        case x : CompilationUnit =>
          var spec : List[String] = List[String]()
          var prog : List[String] = List[String]()
          var classes : Set[String] = Set[String]()
          var pname : Option[String] = None
          var todo : Stack[AbstractTypeDeclaration] = Stack[AbstractTypeDeclaration]()
          todo = todo.pushAll(scala.collection.JavaConversions.asBuffer(x.types).map(_.asInstanceOf[AbstractTypeDeclaration]))
          while (!todo.isEmpty) {
            val t = todo.top
            todo = todo.pop
            t match {
              case x : TypeDeclaration =>
                //Console.println("got typedecl " + x.getName.getIdentifier)
                if (pname == None) pname = Some(x.getName.getIdentifier)
                classes += x.getName.getIdentifier
                todo = todo.pushAll(x.getTypes)
                val p = x.getProperty(EclipseJavaASTProperties.coqDefinition)
                if (p != null)
                  prog ::= p.asInstanceOf[String]
                for (x <- x.getMethods) {
                  //Console.println("setting offset for method [" + x.getName.getIdentifier + "]: " + spec.mkString("\n").length)
                  x.setProperty(EclipseJavaASTProperties.coqOffset, spec.mkString("\n").length)
                  val sp = x.getProperty(EclipseJavaASTProperties.coqSpecification)
                  if (sp != null)
                    spec ::= sp.asInstanceOf[String]
                  val p = x.getProperty(EclipseJavaASTProperties.coqDefinition)
                  if (p != null)
                    prog ::= p.asInstanceOf[String]
                  //Console.println("  got m " + x.getName.getIdentifier)
                }
              case y =>
            }
          }

          val n = pname.get
          val clazz = classes.foldRight("(SM.empty _)")((x, y) => "(SM.add \"" + x + "\" " + x + " " + y + ")")
          val pr = "Definition Prog := Build_Program " + clazz + "."
          val umn = """
Definition unique_method_names := option_proof (search_unique_names Prog).
Opaque unique_method_names."""
          prog = List("""
Require Import AbstractAsn.
Require Import Tactics.

Open Scope string_scope.
Open Scope hasn_scope.

Module """ + n + " <: PROGRAM.") ++ prog ++ List(pr, umn, "End " + n + ".")

          val specpre = """
Module """ + n + """_spec.
Import """ + n + """.
Require Import """ + n + """_model.
Module Import SC := Tac """ + n + """.

Open Scope cmd_scope.
Open Scope spec_scope.
Open Scope asn_scope.
"""
          spec = List(specpre) ++ spec.reverse
          val p = prog.mkString("\n") + "\n"
          val s = spec.mkString("\n") + "\n"

          x.setProperty(EclipseJavaASTProperties.specOffset, p.length + specpre.length + 1)
          x.setProperty(EclipseJavaASTProperties.coqDefinition, p)
          x.setProperty(EclipseJavaASTProperties.coqSpecification, s)
          x.setProperty(EclipseJavaASTProperties.coqEnd, "End " + n + "_spec.")
        case _ =>
      }

    import org.eclipse.jdt.core.dom.{AssertStatement, Assignment, Block, BooleanLiteral, ClassInstanceCreation, EmptyStatement, ExpressionStatement, FieldAccess, IfStatement, MethodInvocation, NullLiteral, NumberLiteral, QualifiedName, ReturnStatement, SimpleName, Statement, StringLiteral, StructuralPropertyDescriptor, VariableDeclarationStatement, WhileStatement}
    private def getBodyString (b : Statement) : Option[String] =
      b match {
        case x : Block =>
          val bod = scala.collection.JavaConversions.asBuffer(x.statements).map(_.asInstanceOf[Statement]).toList
          val bs = bod.map(getBodyString(_))
          Some(printB(bs.flatMap(x => x)))
        case x : ExpressionStatement =>
          val e = x.getExpression
          val res =
            e match {
              case x : Assignment =>
                assert(x.getOperator == Assignment.Operator.ASSIGN)
                val r = x.getRightHandSide
                val l = x.getLeftHandSide
                l match {
                  case y : FieldAccess =>
                    "(cwrite " + printE(y) + " " + printE(r) + ")"
                  case y : QualifiedName =>
                    "(cwrite " + printE(y) + " " + printE(r) + ")"
                  case y =>
                    if (y.isInstanceOf[SimpleName] && isField(y.asInstanceOf[SimpleName]))
                      "(cwrite \"this\" " + printE(y) + " " + printE(r) + ")"
                    else
                      r match {
                        case y : MethodInvocation =>
                          val st = (y.resolveMethodBinding.getModifiers & Modifier.STATIC) == Modifier.STATIC
                          val str = if (st) "cscall" else "cdcall"
                          deps = deps + y.getName.getIdentifier
                          "(" + str + " " + printE(l) + " " + printE(y) + ")"
                        case y : ClassInstanceCreation =>
                          "(calloc " + printE(l) + " " + printE(y) + ")"
                        case y : FieldAccess =>
                          "(cread " + printE(l) + " " + printE(y) + ")"
                        case y : QualifiedName =>
                          "(cread " + printE(l) + " " + printE(y) + ")"
                        case y : SimpleName =>
                          if (isField(y))
                            "(cread " + printE(l) + " \"this\" " + printE(y) + ")"
                          else
                            "(cassign " + printE(l) + " " + printE(y) + ")"
                        case y =>
                          "(cassign " + printE(l) + " " + printE(y) + ")"
                      }
                }
              case x : MethodInvocation =>
                val st = (x.resolveMethodBinding.getModifiers & Modifier.STATIC) == Modifier.STATIC
                val str = if (st) "cscall" else "cdcall"
                deps = deps + x.getName.getIdentifier
                "(" + str + " \"\" " + printE(x) + ")"
            }
          Some(res)
        case x : IfStatement =>
          val tst = printE(x.getExpression)
          val alt =
            if (x.getElseStatement == null)
              Some("(cskip)")
            else
              getBodyString(x.getElseStatement)
          val con = getBodyString(x.getThenStatement)
          Some("(cif " + tst + " " + con.get + " " + alt.get + ")")
        case x : AssertStatement =>
          val ass = printE(x.getExpression)
          Some("(cassert " + ass + ")")
        case x : EmptyStatement =>
          None
        case x : WhileStatement =>
          val tst = printE(x.getExpression)
          val bod = getBodyString(x.getBody)
          Some("(cwhile " + tst + " " + bod.get + ")")
        case x : ReturnStatement =>
          val e = x.getExpression
          //this is special!
          if (! (e.isInstanceOf[SimpleName] || e.isInstanceOf[BooleanLiteral] ||
                 e.isInstanceOf[NullLiteral] || e.isInstanceOf[NumberLiteral] ||
                 e.isInstanceOf[StringLiteral]))
            reportError("only simple names and literals are supported as return", x)
          if (ret != "`0") //avoid multiple return statements
            reportError("only a single return statement is supported!", x)
          if (e.isInstanceOf[BooleanLiteral] ||
              e.isInstanceOf[NullLiteral] || e.isInstanceOf[NumberLiteral] ||
              e.isInstanceOf[StringLiteral])
            ret = printE(e)
          else //if e.isInstanceOf[SimpleName]
            ret = "(var_expr " + printE(e) + ")"
          None
        case x : VariableDeclarationStatement =>
          val frags = x.fragments
          if (frags.size == 1) {
            val frag = frags.get(0).asInstanceOf[VariableDeclarationFragment]
            val nam = printE(frag.getName)
            val init = frag.getInitializer
            if (init != null) {
              val inite = printE(init)
              val ass =
                init match {
                  case y : MethodInvocation =>
                    val st = (y.resolveMethodBinding.getModifiers & Modifier.STATIC) == Modifier.STATIC
                    val str = if (st) "cscall" else "cdcall"
                    deps = deps + y.getName.getIdentifier
                    "(" + str + " " + nam + " " + inite + ")"
                  case y : ClassInstanceCreation =>
                    "(calloc " + nam + " " + inite + ")"
                  case y : QualifiedName =>
                    "(cread " + nam + " " + inite + ")"
                  case y : FieldAccess =>
                    "(cread " + nam + " " + inite + ")"
                  case y : SimpleName =>
                    if (isField(y))
                      "(cread " + nam + " \"this\" " + inite + ")"
                    else
                      "(cassign " + nam + " " + inite + ")"
                  case y =>
                    "(cassign " + nam + " " + inite + ")"
                }
              Some(ass)
            } else None
          } else {
            if (frags.size != 0)
              Console.println("wtf am I supposed do to here?")
            None
          }
      }

    private def printB (bs : List[String]) : String =
      bs match {
        case Nil => "(cskip)"
        case x :: Nil => x
        case x :: y => "(cseq " + x + " " + printB(y) + ")"
      }

    import org.eclipse.jdt.core.dom.InfixExpression
    private def translateop (op : Any) : String =
      op match {
        case InfixExpression.Operator.TIMES => "vtimes"
        case InfixExpression.Operator.PLUS => "vadd"
        case InfixExpression.Operator.MINUS => "vminus"
        case InfixExpression.Operator.LESS => "vlt"
        case InfixExpression.Operator.LESS_EQUALS => "vle"
        case InfixExpression.Operator.GREATER => "vgt"
        case InfixExpression.Operator.GREATER_EQUALS => "vge"
        case InfixExpression.Operator.EQUALS => "veq" //XXX: sometimes eeq_ptrs!?
        case x =>
          Console.println("translating " + op + " to =")
          "="
      }

    private def findClass (x : ASTNode) : TypeDeclaration =
      x match {
        case y : TypeDeclaration => y
        case y => findClass(y.getParent)
      }

    import org.eclipse.jdt.core.dom.{Expression, ThisExpression}
    private def printEev (x : Expression) : String =
      x match {
        //only true for variable names... bindings...
        case y : SimpleName => "\"" + y.getIdentifier + "\"/V"
        //literals
        case y : BooleanLiteral => printE(y)
        case y : NullLiteral => printE(y)
        case y : NumberLiteral => printE(y)
        case y : StringLiteral => printE(y)
        case y : ThisExpression => "\"this\""
        case y => Console.println("dunno how to print (" + y.getClass.toString + ")" + y); ""
      }

    import org.eclipse.jdt.core.dom.ParenthesizedExpression
    private def printE (e : Expression) : String = {
      //Console.println("printE with (" + e.getClass.toString + ") " + e);
      val r =
      e match {
        case x : BooleanLiteral =>
          val v = x.booleanValue
          if (v)
            "`true"
          else
            "`false"
        case x : ClassInstanceCreation =>
          val n = x.getType.toString
          val a = scala.collection.JavaConversions.asBuffer(x.arguments)
          if (a.size != 0) //for now!
            reportError("for now only constructors without arguments are supported", x)
          //val as = a.map(printE(_)).mkString("[", "; ", "]")
          "\"" + n + "\""
        case x : FieldAccess =>
          val e = x.getExpression
          val n = x.getName
          printEev(e) + " " + printE(n)
        case x : InfixExpression =>
          val l = x.getLeftOperand
          val r = x.getRightOperand
          val op = x.getOperator
          val (opstr, opend) =
            if (op == InfixExpression.Operator.NOT_EQUALS)
              ("enot (lift2 eeq_ptr_up", ")")
            else
              ("lift2 " + translateop(op), "")
          "(" + opstr + " " + printEev(l) + " " + printEev(r) + opend + ")"
        case x : MethodInvocation =>
          val n = x.getName
          val e = x.getExpression
          val a = scala.collection.JavaConversions.asBuffer(x.arguments).map(_.asInstanceOf[Expression])
          val as = a.map(printE(_)).mkString("[", "; ", "]")
          val st = (x.resolveMethodBinding.getModifiers & Modifier.STATIC) == Modifier.STATIC
          val expr =
            if (e != null)
              printE(e)
            else
              if (st)
                printE(findClass(x).getName)
              else
                "\"this\""
          deps = deps + n.getIdentifier
          expr + " " + printE(n) + " " + as
        case x : SimpleName =>
          "\"" + x.getIdentifier + "\""
        case x : QualifiedName =>
          printE(x.getQualifier) + " " + printE(x.getName)
        case x : NullLiteral =>
          "`null"
        case x : NumberLiteral =>
          "`" + x.getToken
        case x : ParenthesizedExpression =>
          printE(x.getExpression)
        case x : StringLiteral =>
          "\"" + x.getLiteralValue + "\""
        case x : ThisExpression =>
          "`this"
        case x =>
          Console.println("dunno how to print (" + x.getClass.toString + "): " + x)
          ""
      }
      //Console.println("result is " + r)
      r
    }
  }


}

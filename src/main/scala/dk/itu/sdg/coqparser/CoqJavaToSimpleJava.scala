/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.coqparser

trait SExpression { }
trait Atom extends SExpression
case class SAtom (x : String) extends Atom
case class NAtom (x : Int) extends Atom
case class SList (x : List[SExpression]) extends SExpression

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.ImplicitConversions

import dk.itu.sdg.javaparser._

trait TransformCoqJavaToSimpleJava extends StdTokenParsers {

  def transdefinition (x : Any) = x match {
    case "Definition"~(id : List[String])~t =>
      //Console.println("found definition " + id + " to " + t);
      val s = transform(t)
      Console.println(" " + id(0) + " ++> transformed to " + s)
      assert(id.length == 1)
      if (s.length == 1) {
        val c = transformCode(s(0))
        Console.println("     ++++++>>>> " + c)
        VernacularDefinitions.defs += id(0) -> c
      } else
        if (id(0) == "Spec") {
          //TM.add (class name) (SM mname (arg, spec)
          Console.println("spec definition" + "TM.add (TClass name) (SM.add method (arg, spec))")
        } else {
        val p =
        s(0) match {
          case SAtom("Build_Method") => //args, lvar, body, return
            assert(s(1).isInstanceOf[SList])
            val args = s(1).asInstanceOf[SList].x.filterNot(x => x == SAtom("::")).dropRight(1).map(x => JArgument(x.asInstanceOf[SAtom].x.drop(1).dropRight(1), "int"))
            val lv = s(2).asInstanceOf[SList].x.filterNot(x => x == SAtom("::")).dropRight(1).map(x => JBinding(x.asInstanceOf[SAtom].x.drop(1).dropRight(1), "int", None))
            val m = JMethodDefinition(id(0), "void", args, lv ++ List(VernacularDefinitions.defs(s(3).asInstanceOf[SAtom].x).asInstanceOf[JBodyStatement], JReturn(JVariableAccess(s(4).asInstanceOf[SList].x(1).asInstanceOf[SAtom].x.drop(1).dropRight(1))))) //void is wrong here!
            VernacularDefinitions.defs += id(0) -> m
            m
          case SAtom("Build_Class") => //super, fields, methods
            val bod = findDefs(s(3))
            ClassTable.registerClass(id(0), None, false)
            //ClassTable.addMethod()
            JClassDefinition(Set(), id(0), "", List[String](), bod, None)
          case SAtom("Build_Program") => //class, interfaces
            "Program, well, dunno yet"
          case SAtom("Build_spec") => //unit, fun
            val prepo = s(2).asInstanceOf[SList].x(3).asInstanceOf[SList].x
            //separator is now ","
            val sep = prepo.findIndexOf(x => x == SAtom(","))
            var pre = prepo.slice(0, sep)
            val pos = prepo.slice(sep + 1, prepo.length)
            Console.println("pre " + sexpLS(pre))
            Console.println("pos " + sexpLS(pos))
            "arg?, (anonfun => (pre), (post))"
          case x => "dunno: " + x
        }
        Console.println(p)
      }
      new ~(id, t)
    case a => a
  }

  def sexpLS (x : List[SExpression]) : String = {
    x.map(sexpString).reduceLeft(_ + " " + _)
  }

  def sexpString (x : SExpression) : String = {
    x match {
      case SList(x) => "(" + x.map(sexpString).reduceLeft(_ + " " + _) + ")"
      case SAtom(x) => x
      case NAtom(x) => x.toString
    }
  }

  def findDefs (x : SExpression) : List[JMethodDefinition] = {
    x match {
      case SList(x) =>
        if (x.length == 2)
          List[JMethodDefinition]() //SM.empty _
        else {
          //SM.add name ident rest
          assert(x.length == 4)
          val m = VernacularDefinitions.defs(x(2).asInstanceOf[SAtom].x).asInstanceOf[JMethodDefinition]
          JMethodDefinition(x(1).asInstanceOf[SAtom].x.drop(1).dropRight(1), "void", m.parameters, m.body) :: findDefs(x(3)) //void is wrong here!
        }
    }
  }

  def transform (x : Any) : List[SExpression] = {
    x match {
      case "("~(xs:List[Any])~")" => List(SList(xs.map(transform).flatten))
      case (xs:List[Any]) =>
        if (xs.length == 1)
          transform(xs(0))
        else
          xs.map(transform).flatten
      case (x:String) => List(SAtom(x))
      case (x:Int) => List(NAtom(x))
      case x~"."~y => List(SAtom(x + "." + y))
      case "%"~y => List(SAtom("%" + y))
      case ":"~y => List(SAtom(":" + y))
      case x =>
        Console.println("dunno about (class:" + x.asInstanceOf[AnyRef].getClass + ") " + x)
        List(SList(List[SExpression]()))
    }
  }

  def transformE (c : SExpression) : JExpression = {
    transformCode(c).asInstanceOf[JExpression]
  }

  def transformS (c : SExpression) : String = {
    val v = transformCode(c)
    assert(v.isInstanceOf[JVariableAccess])
    v.asInstanceOf[JVariableAccess].variable
  }

  def transformCode (c : SExpression) : JBodyStatement = {
    c match {
      case SList(xs) => xs(0) match {
        case SAtom("cif") =>
          assert(xs.length == 4)
          JConditional(transformE(xs(1)),
                       transformCode(xs(2)),
                       transformCode(xs(3)))
        case SAtom("egt") =>
          assert(xs.length == 3)
          JBinaryExpression(">", transformE(xs(1)), transformE(xs(2)))
        case SAtom("eminus") =>
          assert(xs.length == 3)
          JBinaryExpression("-", transformE(xs(1)), transformE(xs(2)))
        case SAtom("etimes") =>
          assert(xs.length == 3)
          JBinaryExpression("*", transformE(xs(1)), transformE(xs(2)))
        case SAtom("cseq") =>
          assert(xs.length == 3)
          JBlock(List(transformCode(xs(1)), transformCode(xs(2))))
        case SAtom("ccall") =>
          assert(xs.length == 6)
          //ccall ret var meth arglist class
          //-> var is of type class
          val va = transformS(xs(2))
          val fu = transformS(xs(3))
          assert(xs(4).isInstanceOf[SList])
          val as = xs(4).asInstanceOf[SList].x.filterNot(x => x == SAtom("::")).dropRight(1).map(transformE)
          val re = transformS(xs(1))
          JAssignment(re, JCall(JVariableAccess(va), fu, as)) //TODO: Not sure if JVariableAccess is correct
        case SAtom("cassign") =>
          assert(xs.length == 3)
          JAssignment(transformS(xs(1)), transformE(xs(2)))
        case SAtom("var_expr") =>
          assert(xs.length == 2)
          JVariableAccess(transformS(xs(1)))
        //cread, cwrite
        //calloc
      }
      case NAtom(x) => JLiteral(x.toString)
      case SAtom(mx:String) =>
        if (mx.charAt(0) == '\"')
          JVariableAccess(mx.dropRight(1).drop(1))
        else {
          Console.println("dunno " + mx)
          null
        }
    }
  }

}

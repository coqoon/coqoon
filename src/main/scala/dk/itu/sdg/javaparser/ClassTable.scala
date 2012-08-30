/* (c) 2010-2011 Hannes Mehnert */

package dk.itu.sdg.javaparser

object SJTable {
  import scala.collection.immutable.HashMap
  var ct : HashMap[String, SJDefinition] = HashMap[String, SJDefinition]()

  def addClass (clazz : SJDefinition) = {
    val name = clazz.id
    assert(! ct.contains(name))
    ct = ct + (name -> clazz)
  }

  def getClass (name : String) : SJDefinition = {
    assert(ct.contains(name))
    ct(name)
  }

  def getConstructor(clazz: String) : Option[SJConstructorDefinition] = {
    //TODO: multiple constructors with different arguments (types/amount)
    getClass(clazz).body.filter(_.isInstanceOf[SJConstructorDefinition])
                        .map(_.asInstanceOf[SJConstructorDefinition])
                        .headOption
  }

  def getMethodInClass (clazz : String, method : String) : Option[SJMethodDefinition] = {
    //TODO: multiple methods with different arguments (types/amount)
    getClass(clazz).body.filter(_.isInstanceOf[SJMethodDefinition])
                        .map(_.asInstanceOf[SJMethodDefinition])
                        .filter(_.id == method)
                        .headOption
  }

  def getMethodTypeOfClass (name : String, method : String) : String = {
    getMethodInClass(name, method) match {
      case None => ""
      case Some(x) => x.jtype
    }
  }

  def reset () = { ct = HashMap[String, SJDefinition]() }
}

/*
 * some useful introspection helper for return types of builtin-methods

  import java.lang.{ Class, ClassNotFoundException }
  import java.lang.reflect._

  def getJClass (name : String) : Class[_] = {
    name match {
      case "byte" => java.lang.Byte.TYPE
      case "short" => java.lang.Short.TYPE
      case "int" => java.lang.Integer.TYPE
      case "long" => java.lang.Long.TYPE
      case "float" => java.lang.Float.TYPE
      case "double" => java.lang.Double.TYPE
      case "boolean" => java.lang.Boolean.TYPE
      case "char" => java.lang.Character.TYPE
      case x => try Class.forName(name) catch {
        case e : ClassNotFoundException =>
          try Class.forName("java.lang." + name) catch {
            case e : ClassNotFoundException =>
              try Class.forName("java.util." + name) catch {
                case e : Throwable =>
                  if (name.contains("<")) //poor Java, no generics at RT
                    getJClass(name.substring(0, name.indexOf("<")))
                  else {
                    log.warning("Can't find class for " + name)
                    getJClass("Object")
                  }
              }
          }
      }
    }
  }

      val meth = getJClass(mclass).getMethod(mname, args.map(getJClass) : _*)
      if (mclass.contains("<")) {
        val ty = mclass.substring(mclass.indexOf("<") + 1, mclass.lastIndexOf(">"))
        assert(ty.contains(",") == false) //not multiple TVs
        val tvs = meth.getDeclaringClass.getTypeParameters
        assert(tvs.length == 1)
        if (meth.getGenericReturnType.toString == tvs(0).toString)
          ty
        else {
          log.warning("dunno how to deal with type parameters in here: " + meth.getGenericReturnType.toString)
          meth.getGenericReturnType.toString
        }
      } else
        meth.getReturnType.getName
*/


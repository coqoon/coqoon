/* (c) 2011 Mads Hartmann Jensen */

package dk.itu.sdg.javaparser

sealed abstract class JModifier()

case class Private()      extends JModifier
case class Protected()    extends JModifier
case class Public()       extends JModifier
case class Final()        extends JModifier
case class Abstract()     extends JModifier
case class Static()       extends JModifier
case class Native()       extends JModifier
case class Transient()    extends JModifier
case class Volatile()     extends JModifier
case class Synchronized() extends JModifier
case class Strict()       extends JModifier

object JModifier extends JavaTerms {
      
  def apply(str: String): Option[JModifier] = str match {
    case "private"      => Some(Private())
    case "protected"    => Some(Protected())
    case "public"       => Some(Public())
    case "final"        => Some(Final())
    case "abstract"     => Some(Abstract())
    case "static"       => Some(Static())
    case "native"       => Some(Native())
    case "transient"    => Some(Transient())
    case "volatile"     => Some(Volatile())
    case "synchronized" => Some(Synchronized())
    case "strict"       => Some(Strict())
    case _              => None
  }
  
  def unapply(modifier: JModifier): Option[String] = modifier match {
    case Private()      => Some("private")
    case Protected()    => Some("protected")
    case Public()       => Some("public")
    case Final()        => Some("final")
    case Abstract()     => Some("abstract")
    case Static()       => Some("static")
    case Native()       => Some("native")
    case Transient()    => Some("transient")
    case Volatile()     => Some("volatile")
    case Synchronized() => Some("synchronized")
    case Strict()       => Some("strict")
    case _              => None
  }
}


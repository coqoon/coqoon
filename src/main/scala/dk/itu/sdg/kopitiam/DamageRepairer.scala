/* (c) 2012 Hannes Mehnert */

package dk.itu.sdg.kopitiam

import org.eclipse.jface.text.rules.ITokenScanner
import dk.itu.sdg.kopitiamaspects.DamagerRepairerFactoryInterface

class KopitiamDamagerRepairerFactory extends DamagerRepairerFactoryInterface {
  override def newDamagerRepairer (scanner : ITokenScanner) : KopitiamDamagerRepairer = {
    new KopitiamDamagerRepairer(CoqTokenScanner)
  }
}

object KopitiamDamagerRepairerFactory extends KopitiamDamagerRepairerFactory { }

import org.eclipse.jface.text.rules.DefaultDamagerRepairer

class KopitiamDamagerRepairer (scanner : ITokenScanner) extends DefaultDamagerRepairer (scanner) {
  private var editor : CoqEditor = null; //.:><- sth with getSource...

  def setEditor (e : CoqEditor) : Unit = { editor = e }

  import org.eclipse.swt.graphics.Color
  import org.eclipse.swt.widgets.Display
  import org.eclipse.jface.text.{ ITypedRegion, IRegion, Region, TextPresentation, DocumentEvent }
  import org.eclipse.swt.custom.StyleRange

  private var greenI : Int = -1
  private var yellowI : Int = -1
  def addColors (lengthOfSent : Int, lengthOfProcessed : Int, reveal : Boolean) : Unit = {
    Console.println("damage repairer adding colors " + lengthOfSent + " proc " + lengthOfProcessed)
    val end = lengthOfSent + lengthOfProcessed
    val repr = new TextPresentation(new Region(0, end), end + 1)
    if (lengthOfSent > 0) {
      repr.addStyleRange(new StyleRange(0, lengthOfSent, null, getColor("coqSentBg")))
      greenI = lengthOfSent
    } else
      greenI = -1
    if (lengthOfProcessed > 0) {
      repr.addStyleRange(new StyleRange(lengthOfSent, lengthOfProcessed, null, getColor("coqSentProcessBg")))
      yellowI = lengthOfProcessed
    } else
      yellowI = -1
    if (editor != null)
          Display.getDefault.syncExec(
            new Runnable() {
              def run() = {
                editor.getSource.invalidateTextPresentation
                editor.getSource.changeTextPresentation(repr, true)
                if (reveal)
                  editor.selectAndReveal(lengthOfSent, 0)
              }
            })
  }

  private def getColor (name : String) : Color = {
    import org.eclipse.jface.preference.PreferenceConverter
    val store = Activator.getDefault.getPreferenceStore
    new Color(Display.getDefault, PreferenceConverter.getColor(store, name))
  }

  override def createPresentation (pres : TextPresentation, region : ITypedRegion) : Unit = {
    //Console.println("creating presentation regionoff " + region.getOffset + " len " + region.getLength + "(pres " + pres.getExtent.getOffset + " len " + pres.getExtent.getLength + ")")
    super.createPresentation(pres, region)
    if (greenI > 0 && region.getOffset <= greenI) {
      val realstart = region.getOffset
      val reallen = scala.math.min(greenI - region.getOffset, region.getLength)
      //Console.println("green from " + realstart + " length " + reallen)
      pres.mergeStyleRange(new StyleRange(realstart, reallen, null, getColor("coqSentBg")))
    }
    if (yellowI > 0 && region.getOffset + region.getLength >= greenI) {
      val realstart = scala.math.max(greenI - region.getOffset, region.getOffset)
      val reallen =  scala.math.min(yellowI - region.getOffset, region.getLength)
      //Console.println("yellow from " + realstart + " length " + reallen)
      pres.mergeStyleRange(new StyleRange(realstart, reallen, null, getColor("coqSentProcessBg")))
    }
  }
}



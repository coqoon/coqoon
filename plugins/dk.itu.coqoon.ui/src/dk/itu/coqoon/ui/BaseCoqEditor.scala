/* (c) 2010-2011 Hannes Mehnert and David Christiansen
 * Copyright © 2014 Alexander Faithfull */

package dk.itu.coqoon.ui

import dk.itu.coqoon.core.utilities.TryCast
import org.eclipse.ui.editors.text.TextEditor

abstract class BaseCoqEditor extends TextEditor {
  import org.eclipse.ui.editors.text.EditorsUI
  import org.eclipse.ui.texteditor.ChainedPreferenceStore
  setPreferenceStore(new ChainedPreferenceStore(Array(
      Activator.getDefault.getPreferenceStore,
      EditorsUI.getPreferenceStore)))

  import org.eclipse.jface.text.source.{
    Annotation, ISourceViewer, IVerticalRuler}
  import org.eclipse.jface.text.Position
  import org.eclipse.jface.text.source.projection.{
    ProjectionAnnotation, ProjectionAnnotationModel}

  import dk.itu.coqoon.core.model._
  import dk.itu.coqoon.core.utilities.CacheSlot
  import org.eclipse.ui.IFileEditorInput
  protected[ui] val workingCopy = CacheSlot[Option[IDetachedCoqVernacFile]] {
    TryCast[IFileEditorInput](getEditorInput).map(_.getFile).flatMap(
        ICoqModel.getInstance.toCoqElement).flatMap(
            TryCast[ICoqVernacFile]).map(_.detach)
  }

  import org.eclipse.jface.text.source.SourceViewerConfiguration
  protected def createSourceViewerConfiguration() : SourceViewerConfiguration =
    new BaseCoqSourceViewerConfiguration(this)

  import org.eclipse.ui.editors.text.{
    TextFileDocumentProvider, ForwardingDocumentProvider}
  import org.eclipse.core.filebuffers.IDocumentSetupParticipant

  private object CoqDocumentSetupParticipant
      extends IDocumentSetupParticipant {
    import org.eclipse.jface.text.IDocument
    override def setup(doc : IDocument) =
      CoqPartitions.installPartitioner(doc, CoqPartitions.COQ)
  }

  override protected def initializeEditor() = {
    setDocumentProvider(new ForwardingDocumentProvider(
      CoqPartitions.COQ, CoqDocumentSetupParticipant,
      new TextFileDocumentProvider {
        override def getDefaultEncoding() = "UTF-8"
      }))
    setSourceViewerConfiguration(createSourceViewerConfiguration)
    super.initializeEditor
  }

  protected var annotationModel : Option[ProjectionAnnotationModel] = None
  protected var oldAnnotations : Array[Annotation] = Array()
  protected[ui] def updateFolding() : Unit = {
    import dk.itu.coqoon.core.utilities.Substring

    import scala.collection.JavaConversions._
    var positions : Seq[Position] = Seq()
    workingCopy.get.foreach(_.accept(_ match {
      case f : ICoqScriptGroup
          if f.getChildren.size > 1 =>
        val padding = f.getText.takeWhile(_.isWhitespace).length
        if (Substring(f.getText, padding).count(_ == '\n') < 3) {
          false
        } else {
          positions +:=
            new Position(f.getOffset + padding, f.getLength - padding)
          true
        }
      case f : IParent => true
      case _ => false
    }))

    val newAnnotations = Map(positions.map(
        p => (new ProjectionAnnotation -> p)) : _*)

    annotationModel.foreach(
        _.modifyAnnotations(oldAnnotations, newAnnotations, null))
    oldAnnotations = newAnnotations.map(_._1).toArray
  }

  override def isEditable =
    TryCast[IFileEditorInput](
        getEditorInput).map(_.getFile).flatMap(
           ICoqModel.getInstance.toCoqElement) match {
      case Some(q) =>
        super.isEditable
      case None =>
        /* Anything without a backing ICoqElement isn't really part of Coqoon's
         * world and should be read-only */
        false
    }

  import org.eclipse.ui.texteditor.SourceViewerDecorationSupport
  import org.eclipse.jface.text.source.DefaultCharacterPairMatcher
  override def configureSourceViewerDecorationSupport(
      support : SourceViewerDecorationSupport) = {
    import CoqoonUIPreferences._
    import org.eclipse.jface.text.IDocumentExtension3
    super.configureSourceViewerDecorationSupport(support)
    support.setCharacterPairMatcher(new DefaultCharacterPairMatcher(
        Array('(', ')', '{', '}', '<', '>', '[', ']'),
        CoqPartitions.COQ, true))
    support.setMatchingCharacterPainterPreferenceKeys(
        MATCHING_BRACKETS, MATCHING_BRACKETS_COLOR)
  }

  import org.eclipse.swt.widgets.Composite
  import org.eclipse.jface.text.source.projection.ProjectionViewer

  override def createPartControl(parent : Composite) = {
    import org.eclipse.jface.text.source.projection.ProjectionSupport
    super.createPartControl(parent)

    //Create the necessary infrastructure for code folding
    val projViewer = getSourceViewer.asInstanceOf[ProjectionViewer]
    val projectionSupport = new ProjectionSupport(
        projViewer, getAnnotationAccess(), getSharedColors())
    projectionSupport.install()

    //turn projection mode on
    projViewer.doOperation(ProjectionViewer.TOGGLE)

    annotationModel = Option(projViewer.getProjectionAnnotationModel)
    updateFolding()
  }

  //Create the source viewer as one that supports folding
  override protected def createSourceViewer(parent : Composite,
      ruler : IVerticalRuler, styles : Int) : ISourceViewer = {
    val viewer = new ProjectionViewer(
        parent, ruler, getOverviewRuler(), isOverviewRulerVisible(), styles)
    getSourceViewerDecorationSupport(viewer)
    viewer
  }

  import org.eclipse.ui.views.contentoutline.IContentOutlinePage
  // Support getting outline pages
  var outlinePage : Option[CoqContentOutlinePage] = None
  private def createOutlinePage() : CoqContentOutlinePage = {
    val page = new CoqContentOutlinePage
    workingCopy.get.foreach(page.setInput)
    page
  }

  override def getAdapter(adapter : Class[_]) =
    if (adapter == classOf[ISourceViewer]) {
      getSourceViewer
    } else if (adapter == classOf[IContentOutlinePage]) {
      if (outlinePage == None && getSourceViewer != null)
        outlinePage = Some(createOutlinePage)
      outlinePage.orNull
    } else super.getAdapter(adapter)

  override def initializeKeyBindingScopes =
    setKeyBindingScopes(Array("dk.itu.coqoon.ui.contexts.coq"))
}

import org.eclipse.ui.views.contentoutline.ContentOutlinePage

import dk.itu.coqoon.core.model.{
  ICoqElement, ICoqScriptGroup, ICoqScriptSentence}

class CoqContentOutlinePage extends ContentOutlinePage {
  import org.eclipse.jface.viewers.{
    TreeViewer, SelectionChangedEvent, IStructuredSelection}
  import org.eclipse.swt.widgets.Composite

  override def selectionChanged(event : SelectionChangedEvent) : Unit = {
    super.selectionChanged(event)

    TryCast[IStructuredSelection](
        event.getSelection).flatMap(sel => Option(sel.getFirstElement)) match {
      case Some(g : ICoqScriptGroup) =>
        OpenDeclarationHandler.highlightElement(g.getDeterminingSentence)
      case Some(s : ICoqScriptSentence) =>
        OpenDeclarationHandler.highlightElement(s)
      case _ =>
    }
  }

  override def createControl(parent : Composite) : Unit = {
    super.createControl(parent)

    val viewer = getTreeViewer()
    viewer.setContentProvider(new ModelContentProvider)
    viewer.setLabelProvider(new ModelLabelProvider)
    viewer.setInput(input.orNull)
  }

  private var input : Option[ICoqElement] = None
  def setInput(input : ICoqElement) = {
    this.input = Option(input)
    Option(getTreeViewer).foreach(_.setInput(input))
  }
}
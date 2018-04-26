/* CoqModelImpl.scala
 * An implementation of the abstraction layer between Eclipse and Coq
 * Copyright © 2013 Alexander Faithfull
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations
 * under the License. */

package dk.itu.coqoon.core.model

import dk.itu.coqoon.core.project.{
  CoqProjectFile, CoqProjectEntry, VariableEntry, RecursiveEntry}
import dk.itu.coqoon.core.utilities.{
  TryCast, CacheSlot, TotalReader, BatchCollector}

import org.eclipse.core.runtime.{Path, IPath, IProgressMonitor}
import org.eclipse.core.resources._

private abstract class CoqElementImpl[
    A <: Option[IResource], B <: ICoqElement with IParent](
    private val res : A, private val parent : B) extends ICoqElement {
  protected def properties() : Seq[Any] = Seq(res, parent)

  final override def hashCode() = {
    var result = 17
    properties.foreach(p =>
      result = result * 41 + (if (p == null) 0 else p.hashCode))
    result
  }

  final override def equals(a : Any) =
    Option(a).filter(_.getClass == getClass).flatMap(
        TryCast[CoqElementImpl[_, _]]).exists(_.properties == properties)

  override def toString =
    getClass.getSimpleName + properties.mkString("(", ", ", ")")

  override def getParent = Option(parent)
  override def exists = getCorrespondingResource.exists { _.exists }
  override def getCorrespondingResource = res

  override def getModel() : CoqModelImpl = getAncestor[CoqModelImpl].get

  import CoqEnforcement._
  private var issues : Map[Issue, Severity] = Map()
  override def getIssues() = issues
  override def setIssues(issues : Map[Issue, Severity]) =
    if (this.issues != issues) {
      this.issues = issues
      notifyListeners(CoqIssuesChangedEvent(this))
    }

  protected[model] def notifyListeners(ev : CoqElementEvent) : Unit =
    getModel.notifyListeners(ev)

  override def accept(f : ICoqElement => Boolean) = f(this)
}

import CoqEnforcement._

private[model] object IssueTranslator
    extends BatchCollector[_IssueTranslator.IssueTuple](delay = 200)
        with CoqElementChangeListener {
  override def coqElementChanged(ev : CoqElementEvent) =
    ev match {
      case CoqIssuesChangedEvent(el : ICoqElement) =>
        el.getContainingResource.foreach(r => add(r, el, el.getIssues))
      case CoqElementRemovedEvent(el) =>
        el.getCorrespondingResource.foreach(r => add(r, el, Map()))
      case CoqFileContentChangedEvent(el : ICoqVernacFile) =>
        /* When a file's content is updated, all of its sentences are
         * discarded, so delete their markers as well */
        el.getCorrespondingResource.foreach(r => add(r, el, Map()))
      case _ =>
    }
  override def process(items : List[_IssueTranslator.IssueTuple]) =
    new MarkerUpdateJob(items).schedule
}
private object _IssueTranslator {
  type IssueTuple = (IResource, ICoqElement, Map[Issue, Severity])
}

import dk.itu.coqoon.core.utilities.UniqueRule
import dk.itu.coqoon.core.utilities.JobUtilities.MultiRule
import org.eclipse.core.resources.WorkspaceJob

private class MarkerUpdateJob(tasks : List[_IssueTranslator.IssueTuple])
    extends WorkspaceJob("Update Coq markers") {
  import MarkerUpdateJob._

  setRule(MultiRule(
      rule +: tasks.map(_._1).map(
          ResourcesPlugin.getWorkspace.getRuleFactory.markerRule) : _*))
  setSystem(true)

  import org.eclipse.core.runtime.{Status, IStatus}
  override def runInWorkspace(monitor : IProgressMonitor) : IStatus = {
    for ((r, el, issues) <- tasks) {
      import dk.itu.coqoon.core.ManifestIdentifiers.MARKER_PROBLEM
      val currentMarkers =
        if (r.exists) {
          r.findMarkers(MARKER_PROBLEM, false, IResource.DEPTH_ZERO)
        } else Array[IMarker]()
      import scala.collection.JavaConversions._
      el match {
        case el : ICoqScriptElement =>
          /* As script elements are in some sense immutable, we use their
           * offset in the document as a unique identifier. */
          val elo = el.getOffset
          currentMarkers.foreach(m =>
            if (m.getAttribute(SECRET_OFFSET, Int.MinValue) == elo) {
              m.delete
            })
          issues foreach {
            case (Issue(_, offset, length, message, _), severity) =>
              r.createMarker(MARKER_PROBLEM).setAttributes(Map(
                  IMarker.MESSAGE -> message.trim,
                  IMarker.SEVERITY -> severityToMarkerSeverity(severity),
                  IMarker.LOCATION -> s"offset $offset",
                  IMarker.CHAR_START -> (elo + offset),
                  IMarker.CHAR_END -> (elo + offset + length),
                  IMarker.TRANSIENT -> false,
                  SECRET_OFFSET -> elo))
          }

        case el : ICoqElement if issues.isEmpty =>
          currentMarkers.foreach(_.delete)
        case el : ICoqElement =>
          issues foreach {
            case (Issue(_, offset, length, message, _), severity) =>
              r.createMarker(MARKER_PROBLEM).setAttributes(Map(
                  IMarker.MESSAGE -> message.trim,
                  IMarker.SEVERITY -> severityToMarkerSeverity(severity),
                  IMarker.TRANSIENT -> false))
          }
        case _ =>
      }
    }

    Status.OK_STATUS
  }
}
private object MarkerUpdateJob {
  final val SECRET_OFFSET = "coqoon_secretOffset"
  final val rule = new dk.itu.coqoon.core.utilities.UniqueRule
  def severityToMarkerSeverity(s : Severity) =
    s match {
      case Severity.Error => IMarker.SEVERITY_ERROR
      case Severity.Warning => IMarker.SEVERITY_WARNING
      case _ => IMarker.SEVERITY_INFO
    }
}

private trait ICache {
  /* Clear the cache, in whole or in part, in response to the changes
   * represented by @ev. */
  def update(c : ICache.Change) = destroy
  /* Forget all information stored in the cache. */
  def destroy()
}
object ICache {
  sealed abstract class Change
  case object CoqPathChange extends Change
  case class ResourceChange(d : IResourceDelta) extends Change
}

private abstract class ParentImpl[
    A <: IResource, B <: ICoqElement with IParent](
    private val res : Option[A], private val parent : B)
    extends CoqElementImpl(res, parent) with IParent {
  override def accept(f : ICoqElement => Boolean) =
    if (f(this))
      getChildren.foreach(_.accept(f))
}

private class CoqModelImpl(
    val res : Option[IWorkspaceRoot])
    extends ParentImpl(res, null) with ICoqModel {
  import CoqModelImpl._

  object WorkspaceListener extends IResourceChangeListener {
    class DeltaVisitor(ev : IResourceChangeEvent)
        extends IResourceDeltaVisitor {
      override def visit(d : IResourceDelta) = {
        toCoqElement(d.getResource).foreach(el => d.getKind match {
          case IResourceDelta.ADDED =>
            notifyListeners(CoqElementAddedEvent(el))
          case IResourceDelta.REMOVED =>
            val entry = cache synchronized { cache.get(el) }
            entry.foreach(_.destroy)
            notifyListeners(CoqElementRemovedEvent(el))
          case IResourceDelta.CHANGED =>
            val entry = cache synchronized { cache.get(el) }
            entry.foreach(_.update(ICache.ResourceChange(d)))
          case _ =>
        })
        true
      }
    }

    override def resourceChanged(ev : IResourceChangeEvent) =
        ev.getType() match {
      case IResourceChangeEvent.POST_CHANGE =>
        ev.getDelta().accept(new DeltaVisitor(ev))
      case _ =>
    }
  }
  res.foreach(_.getWorkspace.addResourceChangeListener(
      WorkspaceListener, IResourceChangeEvent.POST_CHANGE))

  import dk.itu.coqoon.core.Activator
  import dk.itu.coqoon.core.CoqoonPreferences
  import org.eclipse.jface.util.IPropertyChangeListener
  object PreferenceListener extends IPropertyChangeListener {
    import org.eclipse.jface.util.PropertyChangeEvent
    override def propertyChange(e : PropertyChangeEvent) =
      if (e.getProperty == CoqoonPreferences.CoqPath.ID)
        getProjects.flatMap(p => getCacheFor(p).toSeq).foreach(
            _.update(ICache.CoqPathChange))
  }
  Activator.getDefault.getPreferenceStore.addPropertyChangeListener(
      PreferenceListener)

  override def properties = Seq(res)

  override def getProject(name : String) =
    new CoqProjectImpl(res.map(_.getProject(name)), this)
  override def getProjects : Seq[CoqProjectImpl] =
    res.toSeq.flatMap(_.getProjects).filter(hasNature).map(
      a => new CoqProjectImpl(Some(a), this))

  override def toCoqElement(resource : IResource) : Option[ICoqElement] =
      resource match {
    case p : IProject if hasNature(p) =>
      Some(getProject(p.getName))
    case f : IFolder if hasNature(f.getProject) =>
      val project = getProject(f.getProject.getName)
      for (i <- project.getLoadPathProviders) i match {
        case SourceLoadPath(src, bin, _) if src.contains(f) =>
          return Some(new CoqPackageFragmentImpl(Some(f),
              new CoqPackageFragmentRootImpl(Some(src), project)))
        case SourceLoadPath(src, Some(bin), _) if bin.contains(f) =>
          return Some(new CoqPackageFragmentImpl(Some(f),
              new CoqPackageFragmentRootImpl(Some(bin), project)))
        case DefaultOutputLoadPath(bin) if bin.contains(f) =>
          return Some(new CoqPackageFragmentImpl(Some(f),
              new CoqPackageFragmentRootImpl(Some(bin), project)))
        case _ =>
      }
      None
    case f : IFile if hasNature(f.getProject) =>
      toCoqElement(f.getParent).flatMap(
          TryCast[CoqPackageFragmentImpl]).flatMap(
        fragment =>
          if (CoqPackageFragmentImpl.isVernacFile(f)) {
            Some(new CoqVernacFileImpl(Some(f), fragment))
          } else if (CoqPackageFragmentImpl.isObjectFile(f)) {
            Some(new CoqObjectFileImpl(Some(f), fragment))
          } else None)
    case _ => None
  }

  override def getChildren = getProjects

  private var cache = scala.collection.mutable.Map[ICoqElement, ICache]()
  protected[model] def getCacheFor[A <: ICache](
      element : ICoqElement, constructor : => A)(implicit a0 : Manifest[A]) =
    cache synchronized {
      cache.getOrElseUpdate(element, constructor).asInstanceOf[A]
    }
  protected[model] def getCacheFor(element : ICoqElement) : Option[ICache] =
    cache synchronized cache.get(element)

  private var listeners : Set[CoqElementChangeListener] = Set()

  override def addListener(l : CoqElementChangeListener) = (listeners += l)
  override def removeListener(l : CoqElementChangeListener) = (listeners -= l)

  override protected[model] def notifyListeners(ev : CoqElementEvent) = {
    import dk.itu.coqoon.core.debug.CoqoonDebugPreferences
    CoqoonDebugPreferences.ModelBroadcasts.log(ev.toString)
    listeners.foreach(_.coqElementChanged(ev))
  }
}
private object CoqModelImpl {
  def hasNature(a : IProject) =
    (a.isOpen && a.getDescription.getNatureIds.exists(ICoqProject.isCoqNature))
}

private class CoqProjectImpl(
    val res : Option[IProject], val parent : ICoqModel)
    extends ParentImpl(res, parent) with ICoqProject {
  private class Cache extends ICache {
    def destroy = Seq(
        projectFile, loadPathProviders, provides, loadPath).foreach(_.clear())

    override def update(change : ICache.Change) : Unit =
      change match {
        case ICache.ResourceChange(delta) =>
          /* XXX: Is this a sensible place to send notifications from? */
          val oldConfig = res.map(_.getFile("_CoqProject").getFullPath)
          val newConfig = res.map(_.getFile(".coqoonProject").getFullPath)
          (oldConfig.flatMap(m => Option(delta.findMember(m))),
              newConfig.flatMap(m => Option(delta.findMember(m)))) match {
            case (_, Some(_)) | (Some(_), None) =>
              destroy
              notifyListeners(
                  CoqProjectLoadPathChangedEvent(CoqProjectImpl.this))
              return
            case _ =>
          }

          /* XXX: This is over-enthusiastic -- only directories in the load
           * path should be considered */
          var hierarchyChanged = false
          object ProjectVisitor extends IResourceDeltaVisitor {
            override def visit(delta : IResourceDelta) : Boolean = {
              if (hierarchyChanged)
                return false
              val kind = delta.getKind &
                  (IResourceDelta.ADDED | IResourceDelta.REMOVED)
              if (delta.getResource.isInstanceOf[IFolder] && kind != 0) {
                hierarchyChanged = true
                false
              } else true
            }
          }
          delta.accept(ProjectVisitor)
          if (hierarchyChanged) {
            /* Only the expanded load path needs to be recomputed */
            loadPath.clear
            notifyListeners(
                CoqProjectLoadPathChangedEvent(CoqProjectImpl.this))
          }
        case ICache.CoqPathChange =>
          loadPath.clear
          notifyListeners(CoqProjectLoadPathChangedEvent(CoqProjectImpl.this))
      }

    import CoqProjectFile._
    private[CoqProjectImpl] final val projectFile =
        CacheSlot[Seq[Seq[String]]] {
      val p = res.map(_.getFile(".coqoonProject"))
      /* Option.exists followed by IResource.exists, since you ask */
      if (p.exists(_.exists)) {
        shellTokeniseWithLines(TotalReader.read(p.get.getContents))
      } else {
        val f = res.map(_.getFile("_CoqProject"))
        if (f.exists(_.exists)) {
          (CoqProjectFile.fromString(
              TotalReader.read(f.get.getContents)).collect {
            case q @ VariableEntry(name, value)
                if name.startsWith("KOPITIAM_") =>
              shellTokenise(value)
          })
        } else Seq()
      }
    }

    private[CoqProjectImpl] final val loadPathProviders =
        CacheSlot[Seq[LoadPathProvider]] {
      def _util(
          lines : Seq[Seq[String]]) : Seq[LoadPathProvider] = {
        val res = CoqProjectImpl.this.res.get
        lines match {
          case Seq("DefaultOutput", bindir) +: tail =>
            DefaultOutputLoadPath(res.getFolder(bindir)) +: _util(tail)
          case Seq("ProjectLoadPath", project) +: tail =>
            ProjectLoadPath(
                res.getWorkspace.getRoot.getProject(project)) +: _util(tail)
          case Seq("SourceLoadPath", srcdir) +: tail =>
            SourceLoadPath(res.getFolder(srcdir)) +: _util(tail)
          case Seq("SourceLoadPath", srcdir, bindir) +: tail =>
            SourceLoadPath(res.getFolder(srcdir),
                Option(res.getFolder(bindir))) +: _util(tail)
          case Seq("SourceLoadPathWithCoqdir", srcdir, coqdir) +: tail =>
            SourceLoadPath(res.getFolder(srcdir),
                None, coqdir.split("\\.").toSeq) +: _util(tail)
          case Seq("SourceLoadPathWithCoqdir",
              srcdir, coqdir, bindir) +: tail =>
            SourceLoadPath(res.getFolder(srcdir),
                Option(res.getFolder(bindir)),
                coqdir.split("\\.").toSeq) +: _util(tail)
          case Seq("ExternalLoadPath", physical) +: tail =>
            ExternalLoadPath(new Path(physical), Nil) +: _util(tail)
          case Seq("ExternalLoadPath", physical, logical) +: tail =>
            ExternalLoadPath(
                new Path(physical), logical.split('.')) +: _util(tail)
          case Seq("AbstractLoadPath", identifier) +: tail =>
              AbstractLoadPath(identifier) +: _util(tail)
          case _ +: tail => _util(tail)
          case Nil => Seq.empty
        }
      }
      projectFile.get match {
        case _ if res == None => Seq()
        case Seq() => List(
          SourceLoadPath(res.get.getFolder("src")),
          DefaultOutputLoadPath(res.get.getFolder("bin")),
          AbstractLoadPath(CoqStandardLibrary.ID))
        case pc => _util(pc)
      }
    }

    private[CoqProjectImpl] final val provides =
        CacheSlot[Seq[String]] {
      def _util(
          lines : Seq[Seq[String]]) : Seq[String] =
        lines match {
          case Seq("Provides", identifier) +: tail =>
            identifier +: _util(tail)
          case _ +: tail => _util(tail)
          case Nil => Seq()
        }
      projectFile.get match {
        case _ if res == None => Seq()
        case Seq() => Seq()
        case pc => _util(pc)
      }
    }

    private[CoqProjectImpl] final val loadPath =
        CacheSlot[Seq[LoadPathEntry]] {
      val localOverrides = getLocalOverrides
      (for (i <- loadPathProviders.get) yield i match {
        case ExternalLoadPath(p, d)
            if localOverrides.contains(p) =>
          ExternalLoadPath(localOverrides.get(p).get, d).getLoadPath
        case p =>
          p.getLoadPath
      }).flatten
    }
  }
  private def getCache() = getModel.getCacheFor(this, new Cache)

  import CoqProjectFile._
  import java.io.ByteArrayInputStream
  private def setProjectConfiguration(
      cfg_ : Seq[Seq[String]], monitor : IProgressMonitor) = if (res != None) {
    val f = res.get.getFile(".coqoonProject")
    if (!cfg_.isEmpty) {
      val cfg =
        cfg_.map(_.map(CoqProjectEntry.escape).mkString(" ")).mkString("\n")
      val contents = new ByteArrayInputStream(cfg.getBytes)
      if (f.exists) {
        f.setContents(contents, IResource.NONE, monitor)
      } else f.create(contents, IResource.HIDDEN, monitor)
    } else if (f.exists) {
      f.delete(IResource.KEEP_HISTORY, monitor)
    }
    /* Virtually everything in the CoqProjectImpl cache depends on the content
     * of the project configuration file, so... */
    getCache.destroy
  }

  override def getLoadPath() = getCache.loadPath.get

  override def getLoadPathProviders : Seq[LoadPathProvider] =
    getCache.loadPathProviders.get

  override def setLoadPathProviders(
      lp : Seq[LoadPathProvider], monitor : IProgressMonitor) =
    setProjectConfiguration(
        lp.map(LoadPathProvider.toConfig) ++
        getProvides.map(providesToSeq), monitor)

  override def getProvides() = getCache.provides.get
  private def providesToSeq(p : String) = Seq("Provides", p)
  override def setProvides(ps : Seq[String], monitor : IProgressMonitor) =
    setProjectConfiguration(
        getLoadPathProviders.map(LoadPathProvider.toConfig) ++
        ps.map(providesToSeq), monitor)

  import dk.itu.coqoon.core.ManifestIdentifiers
  import org.eclipse.core.runtime.Path
  import org.eclipse.core.runtime.QualifiedName
  override def getLocalOverrides() : Map[IPath, IPath] =
    res match {
      case Some(project) =>
        import scala.collection.JavaConversions._
        (for ((from, to) <- project.getPersistentProperties
             if from.getQualifier == ManifestIdentifiers.PLUGIN &&
                from.getLocalName.startsWith("override:"))
          yield (new Path(from.getLocalName.drop("override:".length)) ->
              new Path(to))).toMap
      case _ =>
        Map()
    }
  override def setLocalOverrides(overrides : Map[IPath, IPath]) =
    res.foreach(project => {
      import scala.collection.JavaConversions._
      val names =
        for ((from, to) <- overrides) yield {
          val name = new QualifiedName(
              ManifestIdentifiers.PLUGIN, s"override:${from.toString}")
          project.setPersistentProperty(name,  to.toString)
          name
        }
      val toDelete =
        project.getPersistentProperties.keys.filter(f =>
            f.getQualifier == ManifestIdentifiers.PLUGIN &&
                f.getLocalName.startsWith("override:") && !names.contains(f))
      toDelete.foreach(n => project.setPersistentProperty(n, null))

      /* Err on the side of caution and clear the whole load path */
      getCache.loadPath.clear
    })

  override def getDefaultOutputLocation : Option[IContainer] = {
    for (DefaultOutputLoadPath(folder) <- getLoadPathProviders)
      return Some(folder)
    res.map(_.getFolder("bin"))
  }

  override def getPackageFragmentRoot(folder : IPath) =
    new CoqPackageFragmentRootImpl(res.map(_.getFolder(folder)), this)
  override def getPackageFragmentRoots = getLoadPathProviders.collect {
    case SourceLoadPath(folder, output, _)
        if (res == Some(folder.getProject)) =>
      new CoqPackageFragmentRootImpl(Some(folder), this)
    case DefaultOutputLoadPath(folder)
        if (res == Some(folder.getProject)) =>
      new CoqPackageFragmentRootImpl(Some(folder), this)
  }

  override def getChildren = getPackageFragmentRoots
}

private class CoqPackageFragmentRootImpl(
    val res : Option[IContainer], val parent : ICoqProject)
    extends ParentImpl(res, parent) with ICoqPackageFragmentRoot {
  private def gpfRecurse(res : IContainer) : List[ICoqPackageFragment] = {
    var results = List[ICoqPackageFragment]()
    if (res.exists() && res.getName.matches("^[a-zA-Z0-9-_]+$")) {
      results = results :+ new CoqPackageFragmentImpl(Some(res), this)
      for (i <- res.members; j <- TryCast[IContainer](i))
        results = results ++ gpfRecurse(j)
    }
    results
  }

  override def getPackageFragment(folder : IPath) =
    new CoqPackageFragmentImpl(res.map(_.getFolder(folder)), this)
  override def getPackageFragments = res.toSeq.flatMap(gpfRecurse)

  override def getChildren = getPackageFragments
}

private class CoqPackageFragmentImpl(
    val res : Option[IContainer], val parent : ICoqPackageFragmentRoot)
    extends ParentImpl(res, parent) with ICoqPackageFragment {
  import CoqPackageFragmentImpl._

  override def getCoqdir() =
    (getCorrespondingResource.map(_.getLocation),
     getParent.flatMap(_.getCorrespondingResource).map(_.getLocation)) match {
      case (Some(myLoc), Some(parentLoc)) =>
        Some(myLoc.removeFirstSegments(parentLoc.segmentCount).segments.toSeq)
      case _ => None
    }

  override def getVernacFile(file : IPath) =
    new CoqVernacFileImpl(res.map(_.getFile(file)), this)
  override def getVernacFiles =
    res.toSeq.flatMap(_.members).flatMap(TryCast[IFile]).filter(
        isVernacFile).map(f => new CoqVernacFileImpl(Some(f), this))

  override def getObjectFile(file : IPath) =
    new CoqObjectFileImpl(res.map(_.getFile(file)), this)
  override def getObjectFiles =
    res.toSeq.flatMap(_.members).flatMap(TryCast[IFile]).filter(
        isObjectFile).map(f => new CoqObjectFileImpl(Some(f), this))

  override def getNonCoqFiles =
    res.toSeq.flatMap(_.members).flatMap(TryCast[IFile]).filterNot(
        f => isVernacFile(f) || isObjectFile(f))

  override def getChildren = getVernacFiles ++ getObjectFiles
}
private object CoqPackageFragmentImpl {
  def isVernacFile(a : IFile) =
    Option(a).map(_.getFileExtension).contains("v")
  def isObjectFile(a : IFile) =
    Option(a).map(_.getFileExtension).exists(e => e == "vo" || e == "vio")
}

import java.io.InputStream

private object EmptyInputStream extends InputStream {
  override def read = -1
}

private class CoqVernacFileImpl(
    val res : Option[IFile], val parent : ICoqPackageFragment)
    extends ParentImpl(res, parent) with ICoqVernacFile {
  protected class Cache extends ICache {
    override def destroy = Seq(contents, sentences, groups).foreach(_.clear())

    override def update(change : ICache.Change) =
      change match {
        case ICache.ResourceChange(d) =>
          if ((d.getFlags() & IResourceDelta.CONTENT) != 0) {
            destroy
            notifyListeners(CoqFileContentChangedEvent(CoqVernacFileImpl.this))
          }
        case _ =>
      }

    final val contents = CacheSlot[String] {
      res.map(f => TotalReader.read(f.getContents)).getOrElse("")
    }

    import dk.itu.coqoon.core.coqtop.{CoqSentence, ParserStack}
    final val sentences = CacheSlot[Seq[ICoqScriptSentence with ReparentableCSE]] {
      val content = contents.get
      val sentences = CoqSentence.getNextSentences(content, 0, content.length)
      sentences.map(s => new CoqScriptSentenceImpl(s, CoqVernacFileImpl.this))
    }

    final val groups = CacheSlot[Seq[ICoqScriptElement with ReparentableCSE]] {
      val stack = new ParserStack[ICoqScriptElement with ReparentableCSE, String]()

      def _closeUntil(contextLabel : String) = {
        var context = stack.getInnermostContext
        while (context != Some(contextLabel)) {
          val (tag, body) = stack.popContext
          stack.push(
              new CoqScriptGroupImpl(body.reverse, CoqVernacFileImpl.this))
          context = stack.getInnermostContext
        }
      }
      def innermostIs(s : String) = stack.getInnermostContext.contains(s)

      var sentences = this.sentences.get
      while (sentences != Nil) {
        import dk.itu.coqoon.core.coqtop.CoqSentence.CollectComments
        import dk.itu.coqoon.core.coqtop.CoqSentence.Classifier._
        def _withoutSynthetic(
            tail : Seq[ICoqScriptSentence with ReparentableCSE]) = {
          val (comments, rest) = CollectComments.unapplySeq(tail)
          comments.foreach(stack.push)
          rest
        }
        sentences = sentences match {
          case (h @ SectionStartSentence(id)) :: tail =>
            stack.pushContext(s"section-$id")
            stack.push(h)
            _withoutSynthetic(tail)
          case (h @ IdentifiedEndSentence(id)) :: tail
              if innermostIs(s"section-$id") =>
            stack.push(h)
            val (tag, body) = stack.popContext
            stack.push(new CoqScriptGroupImpl(
                body.reverse, CoqVernacFileImpl.this))
            _withoutSynthetic(tail)

          case (h @ ModuleStartSentence(id)) :: tail =>
            stack.pushContext(s"module-$id")
            stack.push(h)
            _withoutSynthetic(tail)
          case (h @ IdentifiedEndSentence(id)) :: tail
              if innermostIs(s"module-${id}") =>
            stack.push(h)
            val (tag, body) = stack.popContext
            stack.push(
                new CoqScriptGroupImpl(body.reverse, CoqVernacFileImpl.this))
            _withoutSynthetic(tail)

          case (h @ DefinitionSentence(_)) :: tail =>
            stack.push(h)
            _withoutSynthetic(tail)
          case (h @ LtacSentence(_)) :: tail =>
            stack.push(h)
            _withoutSynthetic(tail)
          case (h @ FixpointSentence(_)) :: tail =>
            stack.push(h)
            _withoutSynthetic(tail)
          case (h @ InductiveSentence(_)) :: tail =>
            stack.push(h)
            _withoutSynthetic(tail)

          case (h @ LoadSentence(_)) :: tail =>
            stack.push(h)
            _withoutSynthetic(tail)
          case (h @ RequireSentence(_)) :: tail =>
            stack.push(h)
            _withoutSynthetic(tail)
          case (h @ FromRequireSentence(_)) :: tail =>
            stack.push(h)
            _withoutSynthetic(tail)
          case (h @ DeclareMLSentence(_)) :: tail =>
            stack.push(h)
            _withoutSynthetic(tail)

          case (h @ AssertionSentence(_, id, _)) :: tail =>
            stack.pushContext(s"proof-$id")
            stack.push(h)
            _withoutSynthetic(tail)
          case (h @ ProofEndSentence(_)) ::
               (i @ ProofStartSentence(_)) :: tail =>
            /* This isn't really the end of a proof (perhaps Program is being
             * used?) */
            stack.push(h)
            stack.push(i)
            _withoutSynthetic(tail)

          case (h @ ProofEndSentence(_)) :: tail
              if stack.getContext {
                case f if f.startsWith("proof-") => true
                case _ => false
              } != None =>
            val label =
              (stack.getContext {
                case f if f.startsWith("proof-") => true
                case _ => false
              }).get
            _closeUntil(label)
            stack.push(h)
            val (tag, body) = stack.popContext
            stack.push(
                new CoqScriptGroupImpl(body.reverse, CoqVernacFileImpl.this))
            _withoutSynthetic(tail)

          case (h @ SubproofSentence()) :: tail =>
            stack.pushContext(s"subproof-curly")
            stack.push(h)
            _withoutSynthetic(tail)

          /* If we encounter a "}" (and no new proof has started since we saw
           * the corresponding "{"), then close any intervening subproofs
           * before also closing this one */
          case (h @ EndSubproofSentence()) :: tail
              if stack.getContext {
                case f if f == "subproof-curly" => true
                case f if f.startsWith("proof-") => true
                case _ => false
              } == Some("subproof-curly") =>
            _closeUntil("subproof-curly")
            stack.push(h)
            val (tag, body) = stack.popContext
            stack.push(
                new CoqScriptGroupImpl(body.reverse, CoqVernacFileImpl.this))
            _withoutSynthetic(tail)

          case (h @ BulletSentence(kind)) :: tail =>
            val label = s"bullet-$kind"
            if (stack.getContext {
                  case f if f == label => true
                  case f if f == "subproof-curly" => true
                  case f if f.startsWith("proof-") => true
                  case _ => false
                } == Some(label)) {
              _closeUntil(label)
              val (tag, body) = stack.popContext
              stack.push(
                  new CoqScriptGroupImpl(body.reverse, CoqVernacFileImpl.this))
            }
            stack.pushContext(label)
            stack.push(h)
            _withoutSynthetic(tail)

          /* Something of the form "Foo. Proof." is probably a proof, even if
           * we don't recognise what "Foo." means (unless it's a comment) */
          case h :: (i @ ProofStartSentence(_)) :: tail =>
            /* XXX: scan "h" for a proof identifier? */
            stack.pushContext("proof-baffling")
            stack.push(h)
            stack.push(i)
            _withoutSynthetic(tail)

          case h :: tail =>
            stack.push(h)
            _withoutSynthetic(tail)
          case Nil =>
            Nil
        }
      }

      /* Close any remaining contexts (XXX: should we also create error markers
       * here?) */
      while (stack.getInnermostContext != None) {
        val (tag, body) = stack.popContext
        stack.push(
            new CoqScriptGroupImpl(body.reverse, CoqVernacFileImpl.this))
      }

      stack.getStack.reverse
    }
  }
  protected def getCache() = getModel.getCacheFor(this, new Cache)

  override def getChildren = getCache.groups.get

  override def getObjectFile() : Option[ICoqObjectFile] = {
    import dk.itu.coqoon.core.CoqoonPreferences
    for (m <- getAncestor[ICoqModel];
         r <- getCorrespondingResource;
         p <- getAncestor[ICoqProject];
         SourceLoadPath(src, bin, coqdir) <- p.getLoadPathProviders
             if src.contains(r);
         location = r.getLocation;
         partialObjectPath = location.removeFirstSegments(
             src.getLocation.segmentCount).removeFileExtension;
         objectPath = new Path(coqdir.mkString("/")).append(
             partialObjectPath.addFileExtension(
                 if (CoqoonPreferences.UseQuick.get) "vio" else "vo"));
         output = bin.getOrElse(p.getDefaultOutputLocation.get);
         objectFile = output.getFile(objectPath);
         f <- m.toCoqElement(objectFile).flatMap(
             TryCast[ICoqObjectFile]))
      return Some(f)
    None
  }

  override def getLineOffset(line_ : Int) : Option[Int] = {
    val line = line_ - 1
    import CoqVernacFileImpl.Newline
    val lineOffsets =
      0 +: Newline.findAllMatchIn(getCache.contents.get).toSeq.map(_.end)
    if (line >= 0 && line <= lineOffsets.length) {
      Some(lineOffsets(line))
    } else None
  }

  override def getSentenceAt(offset : Int) : Option[ICoqScriptSentence] = {
    var pos = 0
    for (i <- getCache.sentences.get) {
      val end = pos + i.getLength
      if (offset >= pos && offset < end) {
        return Some(i)
      } else pos = end
    }
    None
  }

  override def detach = new DetachedCoqVernacFileImpl(this)
}
object CoqVernacFileImpl {
  private final val Newline = "\n".r
}

private class DetachedCoqVernacFileImpl(
    val original : CoqVernacFileImpl)
    extends CoqVernacFileImpl(original.res, original.parent)
        with IDetachedCoqVernacFile {
  getCache.contents.get

  lazy val id = Math.random
  override def properties = Seq(original, id)

  import java.io.{ByteArrayInputStream => BAIS}
  override def commit(monitor : IProgressMonitor) =
    original.res.foreach(_.setContents(new BAIS(getContents.getBytes("UTF-8")),
        IResource.KEEP_HISTORY, monitor))

  override def getContents = getCache.contents.get
  override def setContents(contents : String) = {
    getCache.contents.set(Option(contents))
    getCache.sentences.clear
    getCache.groups.clear
    notifyListeners(CoqFileContentChangedEvent(this))
  }
}

private trait ReparentableCSE {
  def reparent(p : ICoqElement with IParent) :
      ICoqScriptElement with ReparentableCSE
}

import dk.itu.coqoon.core.coqtop.CoqSentence.Sentence
import dk.itu.coqoon.core.utilities.Substring
private class CoqScriptSentenceImpl(
    private val sentence : Sentence,
    private val parent : ICoqElement with IParent)
        extends CoqElementImpl(None, parent)
        with ICoqScriptSentence with ReparentableCSE {
  override def properties = super.properties :+ sentence

  override lazy val getText = sentence._1.toString
  override lazy val getOffset = sentence._1.start
  override lazy val getLength = sentence._1.length

  override def isSynthetic = sentence._2

  override def toString = s"(${getClass.getSimpleName})" + getText

  private var entities : Map[(Int, Int), ICoqEntity] = Map()
  override def getEntities = entities
  override def setEntities(entities : Map[(Int, Int), ICoqEntity]) =
    if (entities != this.entities) {
      this.entities = entities
      notifyListeners(CoqEntitiesChangedEvent(this))
    }

  override def reparent(p : ICoqElement with IParent) : CoqScriptSentenceImpl =
    if (p != parent) {
      new CoqScriptSentenceImpl(sentence, p)
    } else this
}

private class CoqScriptGroupImpl(
    private val elements_ : Seq[ICoqScriptElement with ReparentableCSE],
    private val parent : ICoqElement with IParent)
        extends ParentImpl(None, parent)
        with ICoqScriptGroup with ReparentableCSE {
  lazy val elements = elements_.map(_.reparent(this))
  override def getChildren = elements

  override def toString = s"CoqScriptGroupImpl(${getDeterminingSentence})"

  def reparent(p : ICoqElement with IParent) : CoqScriptGroupImpl =
    if (p != parent) {
      new CoqScriptGroupImpl(elements, p)
    } else this
}

private case class CoqObjectFileImpl(
    private val res : Option[IFile],
    private val parent : ICoqPackageFragment)
    extends CoqElementImpl(res, parent) with ICoqObjectFile {
  override def getVernacFiles() : Seq[ICoqVernacFile] =
    for (m <- getAncestor[ICoqModel].toSeq;
         r <- getCorrespondingResource.toSeq;
         p <- getAncestor[ICoqProject].toSeq;
         default = p.getDefaultOutputLocation.get;
         SourceLoadPath(src, bin_, coqdir) <- p.getLoadPathProviders
             if bin_.getOrElse(default).contains(r);
         bin = bin_.getOrElse(default);
         location = r.getLocation;
         partialSourcePath = location.removeFirstSegments(
             bin.getLocation.segmentCount + coqdir.size).removeFileExtension;
         sourcePath = partialSourcePath.addFileExtension("v");
         sourceFile = src.getFile(sourcePath);
         f <- m.toCoqElement(sourceFile).flatMap(
             TryCast[ICoqVernacFile]) if f.exists)
      yield f

  override def isQuick =
    (res.map(_.getLocation.getFileExtension == "vio")).getOrElse(false)
}

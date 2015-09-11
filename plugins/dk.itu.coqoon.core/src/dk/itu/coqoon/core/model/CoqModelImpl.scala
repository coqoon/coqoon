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
import dk.itu.coqoon.core.utilities.{TryCast, CacheSlot, TotalReader}

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

  override def toString = getClass.getSimpleName + "@" +
      hashCode.toHexString + properties.mkString("(", ", ", ")")

  override def getParent = Option(parent)
  override def exists = getCorrespondingResource.exists { _.exists }
  override def getCorrespondingResource = res

  override def getModel() : CoqModelImpl = getAncestor[CoqModelImpl].get

  protected[model] def notifyListeners(ev : CoqElementEvent) : Unit =
    getModel.notifyListeners(ev)

  override def accept(f : ICoqElement => Boolean) = f(this)
}

private trait ICache {
  /* Clear the cache, in whole or in part, in response to the changes
   * represented by @ev. */
  def update(ev : IResourceChangeEvent) = destroy
  /* Forget all information stored in the cache. */
  def destroy()
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
          case _ =>
            val entry = cache synchronized { cache.get(el) }
            entry.foreach(_.update(ev))
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

  override def getProject(name : String) =
    new CoqProjectImpl(res.map(_.getProject(name)), this)
  override def getProjects =
    res.toSeq.flatMap(_.getProjects).filter(hasNature).map(
      a => new CoqProjectImpl(Some(a), this))

  override def toCoqElement(resource : IResource) : Option[ICoqElement] =
      resource match {
    case p : IProject if hasNature(p) =>
      Some(getProject(p.getName))
    case f : IFolder if hasNature(f.getProject) =>
      val project = getProject(f.getProject.getName)
      for (i <- project.getLoadPathProviders) i match {
        case SourceLoadPath(src, bin) if src.contains(f) =>
          return Some(new CoqPackageFragmentImpl(Some(f),
              new CoqPackageFragmentRootImpl(Some(src), project)))
        case SourceLoadPath(src, Some(bin)) if bin.contains(f) =>
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

  private var listeners : Set[CoqElementChangeListener] = Set()

  override def addListener(l : CoqElementChangeListener) = (listeners += l)
  override def removeListener(l : CoqElementChangeListener) = (listeners -= l)

  override protected[model] def notifyListeners(ev : CoqElementEvent) =
    listeners.foreach(_.coqElementChanged(ev))
}
private object CoqModelImpl {
  def hasNature(a : IProject) =
    (a.isOpen && a.getDescription.getNatureIds.exists(ICoqProject.isCoqNature))
}

private class CoqProjectImpl(
    val res : Option[IProject], val parent : ICoqModel)
    extends ParentImpl(res, parent) with ICoqProject {
  private class Cache extends ICache {
    def destroy = Seq(projectFile, loadPathProviders, loadPath).map(_.clear)

    override def update(ev : IResourceChangeEvent) : Unit = {
      /* XXX: Is this a sensible place to send notifications from? */

      val delta = ev.getDelta
      res.map(res => delta.findMember(
          res.getFile("_CoqProject").getFullPath)) match {
        case Some(delta) =>
          destroy
          notifyListeners(CoqProjectLoadPathChangedEvent(CoqProjectImpl.this))
          return
        case None =>
      }

      /* XXX: This is over-enthusiastic -- only directories in the load path
       * should be considered */
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
        notifyListeners(CoqProjectLoadPathChangedEvent(CoqProjectImpl.this))
      }
    }

    import CoqProjectFile._
    private[CoqProjectImpl] final val projectFile =
        CacheSlot[CoqProjectFile] {
      val f = res.map(_.getFile("_CoqProject"))
      /* Option.exists followed by IResource.exists, since you ask */
      if (f.exists(_.exists)) {
        CoqProjectFile.fromString(TotalReader.read(f.get.getContents))
      } else Seq()
    }

    private[CoqProjectImpl] final val loadPathProviders =
        CacheSlot[Seq[LoadPathProvider]] {
      def _util(
        seq : Seq[CoqProjectEntry]) : Seq[LoadPathProvider] = seq match {
        /* XXX: also parse the -R options later? */
        case (q @ VariableEntry(name, value)) :: tail
            if name.startsWith("KOPITIAM_") =>
          val res = CoqProjectImpl.this.res.get
          CoqProjectFile.shellTokenise(value) match {
            case "DefaultOutput" :: bindir :: Nil =>
              DefaultOutputLoadPath(res.getFolder(bindir)) +: _util(tail)
            case "ProjectLoadPath" :: project :: Nil =>
              ProjectLoadPath(
                res.getWorkspace.getRoot.getProject(project)) +: _util(tail)
            case "SourceLoadPath" :: srcdir :: Nil =>
              SourceLoadPath(res.getFolder(srcdir)) +: _util(tail)
            case "SourceLoadPath" :: srcdir :: bindir :: Nil =>
              SourceLoadPath(res.getFolder(srcdir),
                Option(res.getFolder(bindir))) +: _util(tail)
            case "ExternalLoadPath" :: physical :: Nil =>
              ExternalLoadPath(new Path(physical), Nil) +: _util(tail)
            case "ExternalLoadPath" :: physical :: logical :: Nil =>
              ExternalLoadPath(
                  new Path(physical), logical.split('.')) +: _util(tail)
            case "AbstractLoadPath" :: identifier :: Nil =>
              AbstractLoadPath(identifier) +: _util(tail)
            case _ => _util(tail)
          }
        case _ :: tail => _util(tail)
        case Nil => Seq.empty
      }
      projectFile.get match {
        case _ if res == None => Seq()
        case Nil => List(
          SourceLoadPath(res.get.getFolder("src")),
          DefaultOutputLoadPath(res.get.getFolder("bin")),
          AbstractLoadPath(CoqStandardLibrary.ID))
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
      cfg : CoqProjectFile, monitor : IProgressMonitor) = if (res != None) {
    val f = res.get.getFile("_CoqProject")
    if (!cfg.isEmpty) {
      val contents = new ByteArrayInputStream(
        CoqProjectFile.toString(cfg).getBytes)
      if (f.exists) {
        f.setContents(contents, IResource.NONE, monitor)
      } else f.create(contents, IResource.NONE, monitor)
    } else if (f.exists) {
      f.delete(IResource.KEEP_HISTORY, monitor)
    }
    getCache.projectFile.set(Option(cfg))
  }
  private def getProjectConfiguration : CoqProjectFile =
    getCache.projectFile.get

  override def getLoadPath() = getCache.loadPath.get

  override def getLoadPathProviders : Seq[LoadPathProvider] =
    getCache.loadPathProviders.get
  override def setLoadPathProviders(
      lp : Seq[LoadPathProvider], monitor : IProgressMonitor) = {
    var coqPart : List[CoqProjectEntry] = Nil
    var kopitiamPart : List[CoqProjectEntry] = Nil
    var count = 0
    for (i <- lp) {
      kopitiamPart :+= VariableEntry("KOPITIAM_" + count, (i match {
        case AbstractLoadPath(identifier) =>
          Seq("AbstractLoadPath", identifier)
        case DefaultOutputLoadPath(bin) =>
          val path = bin.getProjectRelativePath.toString
          coqPart :+= RecursiveEntry(path, "")
          Seq("DefaultOutput", bin.getProjectRelativePath.toString)
        case ExternalLoadPath(path_, Nil) =>
          val path = path_.toString
          coqPart :+= RecursiveEntry(path, "")
          Seq("ExternalLoadPath", path)
        case ExternalLoadPath(path_, coqdir) =>
          val path = path_.toString
          coqPart :+= RecursiveEntry(path, coqdir.mkString("."))
          Seq("ExternalLoadPath", path, coqdir.mkString("."))
        case ProjectLoadPath(project) =>
          val path = project.getName
          Seq("ProjectLoadPath", project.getName)
        case SourceLoadPath(src, None) =>
          val srcPath = src.getProjectRelativePath.toString
          coqPart :+= RecursiveEntry(srcPath, "")
          Seq("SourceLoadPath", srcPath)
        case SourceLoadPath(src, Some(bin)) =>
          val srcPath = src.getProjectRelativePath.toString
          val binPath = bin.getProjectRelativePath.toString
          coqPart ++= Seq(
            RecursiveEntry(srcPath, ""), RecursiveEntry(binPath, ""))
          Seq("SourceLoadPath", srcPath, binPath)
      }).map(CoqProjectEntry.escape).mkString(" "))
      count += 1
    }
    setProjectConfiguration(coqPart ++ kopitiamPart, monitor)
  }

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

  override def getDefaultOutputLocation : Option[IFolder] = {
    for (DefaultOutputLoadPath(folder) <- getLoadPathProviders)
      return Some(folder)
    res.map(_.getFolder("bin"))
  }

  override def getPackageFragmentRoot(folder : IPath) =
    new CoqPackageFragmentRootImpl(res.map(_.getFolder(folder)), this)
  override def getPackageFragmentRoots = getLoadPathProviders.collect {
    case SourceLoadPath(folder, output)
        if (res == Some(folder.getProject)) =>
      new CoqPackageFragmentRootImpl(Some(folder), this)
    case DefaultOutputLoadPath(folder)
        if (res == Some(folder.getProject)) =>
      new CoqPackageFragmentRootImpl(Some(folder), this)
  }

  override def getChildren = getPackageFragmentRoots
}

private class CoqPackageFragmentRootImpl(
    val res : Option[IFolder], val parent : ICoqProject)
    extends ParentImpl(res, parent) with ICoqPackageFragmentRoot {
  private def gpfRecurse(res : IFolder) : List[ICoqPackageFragment] = {
    var results = List[ICoqPackageFragment]()
    if (res.exists() && res.getName.matches("^[a-zA-Z0-9-_]+$")) {
      results = results :+ new CoqPackageFragmentImpl(Some(res), this)
      for (i <- res.members; j <- TryCast[IFolder](i))
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
    val res : Option[IFolder], val parent : ICoqPackageFragmentRoot)
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
    res.toSeq.flatMap(_.members).collect(fileCollector).filter(
        isVernacFile).map(f => new CoqVernacFileImpl(Some(f), this))

  override def getObjectFile(file : IPath) =
    new CoqObjectFileImpl(res.map(_.getFile(file)), this)
  override def getObjectFiles =
    res.toSeq.flatMap(_.members).collect(fileCollector).filter(
        isObjectFile).map(f => new CoqObjectFileImpl(Some(f), this))

  override def getNonCoqFiles =
    res.toSeq.flatMap(_.members).collect(fileCollector).filterNot(
        f => isVernacFile(f) || isObjectFile(f))

  override def getChildren = getVernacFiles ++ getObjectFiles
}
private object CoqPackageFragmentImpl {
  val fileCollector : PartialFunction[AnyRef, IFile] = {
    case a : IFile => a
  }

  def isVernacFile(a : IFile) = Option(a).map(_.getProjectRelativePath).map(
      _.getFileExtension == "v").getOrElse(false)

  def isObjectFile(a : IFile) = Option(a).map(_.getProjectRelativePath).map(
      _.getFileExtension == "vo").getOrElse(false)
}

import java.io.InputStream

private object EmptyInputStream extends InputStream {
  override def read = -1
}

private class CoqVernacFileImpl(
    val res : Option[IFile], val parent : ICoqPackageFragment)
    extends ParentImpl(res, parent) with ICoqVernacFile {
  protected class Cache extends ICache {
    override def destroy = Seq(sentences).map(_.clear)

    override def update(ev : IResourceChangeEvent) = {
      destroy
      notifyListeners(CoqFileContentChangedEvent(CoqVernacFileImpl.this))
    }

    import dk.itu.coqoon.core.coqtop.{CoqSentence, ParserStack}
    final val sentences = CacheSlot[Seq[ICoqScriptElement]] {
      val content = CoqVernacFileImpl.this.getContents
      var sentences = CoqSentence.getNextSentences(content, 0, content.length)

      val stack = new ParserStack[ICoqScriptElement, String]()

      def _closeUntil(contextLabel : String) = {
        var context = stack.getInnermostContext
        while (context != Some(contextLabel)) {
          val (tag, body) = stack.popContext
          stack.push(
              new CoqScriptGroupImpl(body.reverse, CoqVernacFileImpl.this))
          context = stack.getInnermostContext
        }
      }

      while (sentences != Nil) {
        import CoqSentence.Classifier._
        sentences = sentences match {
          case (h @ (SectionStartSentence(identifier), _)) :: tail =>
            stack.pushContext(s"section-${identifier}")
            stack.push(
                new CoqSectionStartSentenceImpl(h, CoqVernacFileImpl.this))
            tail
          case (h @ (IdentifiedEndSentence(id), _)) :: tail
              if stack.getInnermostContext == Some(s"section-${id}") =>
            stack.push(new CoqScriptSentenceImpl(h, CoqVernacFileImpl.this))
            val (tag, body) = stack.popContext
            stack.push(new CoqScriptGroupImpl(
                body.reverse, CoqVernacFileImpl.this))
            tail

          case (h @ (ModuleStartSentence(identifier), _)) :: tail =>
            stack.pushContext(s"module-${identifier}")
            stack.push(
                new CoqModuleStartSentenceImpl(h, CoqVernacFileImpl.this))
            tail
          case (h @ (IdentifiedEndSentence(id), _)) :: tail
              if stack.getInnermostContext == Some(s"module-${id}") =>
            stack.push(new CoqScriptSentenceImpl(h, CoqVernacFileImpl.this))
            val (tag, body) = stack.popContext
            stack.push(
                new CoqScriptGroupImpl(body.reverse, CoqVernacFileImpl.this))
            tail

          case (h @ (DefinitionSentence(_, identifier, _, _), _)) :: tail =>
            stack.push(
                new CoqDefinitionSentenceImpl(h, CoqVernacFileImpl.this))
            tail
          case (h @ (LtacSentence(identifier, _), _)) :: tail =>
            stack.push(new CoqLtacSentenceImpl(h, CoqVernacFileImpl.this))
            tail
          case (h @ (FixpointSentence(_, identifier, _), _)) :: tail =>
            stack.push(new CoqFixpointSentenceImpl(h, CoqVernacFileImpl.this))
            tail
          case (h @ (InductiveSentence(_, identifier, _), _)) :: tail =>
            stack.push(new CoqInductiveSentenceImpl(h, CoqVernacFileImpl.this))
            tail

          case (h @ (LoadSentence(ident), _)) :: tail =>
            stack.push(new CoqLoadSentenceImpl(h, CoqVernacFileImpl.this))
            tail
          case (h @ (RequireSentence(_, what), _)) :: tail =>
            stack.push(new CoqRequireSentenceImpl(h, CoqVernacFileImpl.this))
            tail

          case (h @ (AssertionSentence(_, identifier, _), _)) :: tail =>
            stack.pushContext(s"proof-${identifier}")
            stack.push(new CoqAssertionSentenceImpl(h, CoqVernacFileImpl.this))
            tail
          case (h @ (ProofEndSentence(_), _)) ::
               (i @ (ProofStartSentence(_), _)) :: tail =>
            /* This isn't really the end of a proof (perhaps Program is being
             * used?) */
            stack.push(new CoqScriptSentenceImpl(h, CoqVernacFileImpl.this))
            stack.push(new CoqScriptSentenceImpl(i, CoqVernacFileImpl.this))
            tail

          case (h @ (ProofEndSentence(_), _)) :: tail
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
            stack.push(new CoqScriptSentenceImpl(h, CoqVernacFileImpl.this))
            val (tag, body) = stack.popContext
            stack.push(
                new CoqScriptGroupImpl(body.reverse, CoqVernacFileImpl.this))
            tail

          case (h @ (SubproofSentence(), _)) :: tail =>
            stack.pushContext(s"subproof-curly")
            stack.push(new CoqScriptSentenceImpl(h, CoqVernacFileImpl.this))
            tail

          /* If we encounter a "}" (and no new proof has started since we saw
           * the corresponding "{"), then close any intervening subproofs
           * before also closing this one */
          case (h @ (EndSubproofSentence(), _)) :: tail
              if stack.getContext {
                case f if f == "subproof-curly" => true
                case f if f.startsWith("proof-") => true
                case _ => false
              } == Some("subproof-curly") =>
            _closeUntil("subproof-curly")
            stack.push(new CoqScriptSentenceImpl(h, CoqVernacFileImpl.this))
            val (tag, body) = stack.popContext
            stack.push(
                new CoqScriptGroupImpl(body.reverse, CoqVernacFileImpl.this))
            tail

          case (h @ (BulletSentence(b), _)) :: tail =>
            val label = s"bullet-$b"
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
            stack.push(new CoqScriptSentenceImpl(h, CoqVernacFileImpl.this))
            tail

          /* Something of the form "Foo. Proof." is probably a proof, even if
           * we don't recognise what "Foo." means (unless it's a comment) */
          case (h @ (_, false)) :: (i @ (ProofStartSentence(_), _)) :: tail =>
            /* XXX: scan "h" for a proof identifier? */
            stack.pushContext("proof-baffling")
            stack.push(new CoqScriptSentenceImpl(h, CoqVernacFileImpl.this))
            stack.push(new CoqScriptSentenceImpl(i, CoqVernacFileImpl.this))
            tail

          case h :: tail =>
            stack.push(new CoqScriptSentenceImpl(h, CoqVernacFileImpl.this))
            tail
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

  /* Called from within the cache! */
  protected def getContents() =
    res.map(f => TotalReader.read(f.getContents)).getOrElse("")

  override def getChildren = getCache.sentences.get

  override def detach = new DetachedCoqVernacFileImpl(this)
}

private class DetachedCoqVernacFileImpl(
    val original : CoqVernacFileImpl)
    extends CoqVernacFileImpl(original.res, original.parent)
        with IDetachedCoqVernacFile {
  private val content = CacheSlot[String](super.getContents)
  content.get

  override def properties = Seq(original)

  import java.io.{ByteArrayInputStream => BAIS}
  override def commit(monitor : IProgressMonitor) =
    original.res.foreach(_.setContents(new BAIS(getContents.getBytes("UTF-8")),
        IResource.KEEP_HISTORY, monitor))

  override def getContents = content.get
  override def setContents(contents : String) = {
    content.set(Option(contents))
    getCache.sentences.clear
    /* XXX: should it be possible to distinguish between multiple detached
     * versions of a single file? */
    notifyListeners(CoqFileContentChangedEvent(this))
  }
}

import dk.itu.coqoon.core.coqtop.CoqSentence.Sentence
import dk.itu.coqoon.core.utilities.Substring
private class CoqScriptSentenceImpl(
    private val sentence : Sentence,
    private val parent : ICoqElement with IParent)
        extends CoqElementImpl(None, parent) with ICoqScriptSentence {
  override def getText = sentence._1.toString
  override def getOffset = sentence._1.start
  override def getLength = sentence._1.length

  override def isSynthetic = sentence._2

  override def toString = s"(${getClass.getSimpleName})" + getText
}

private class CoqLtacSentenceImpl(
    private val sentence : Sentence,
    private val parent : ICoqElement with IParent)
        extends CoqScriptSentenceImpl(sentence, parent) with ICoqLtacSentence

private class CoqFixpointSentenceImpl(
    private val sentence : Sentence,
    private val parent : ICoqElement with IParent)
        extends CoqScriptSentenceImpl(sentence, parent)
            with ICoqFixpointSentence

private class CoqInductiveSentenceImpl(
    private val sentence : Sentence,
    private val parent : ICoqElement with IParent)
        extends CoqScriptSentenceImpl(sentence, parent)
            with ICoqInductiveSentence

private class CoqDefinitionSentenceImpl(
    private val sentence : Sentence,
    private val parent : ICoqElement with IParent)
        extends CoqScriptSentenceImpl(sentence, parent)
            with ICoqDefinitionSentence

private class CoqLoadSentenceImpl(
    private val sentence : Sentence,
    private val parent : ICoqElement with IParent)
        extends CoqScriptSentenceImpl(sentence, parent)
            with ICoqLoadSentence

private class CoqRequireSentenceImpl(
    private val sentence : Sentence,
    private val parent : ICoqElement with IParent)
        extends CoqScriptSentenceImpl(sentence, parent)
            with ICoqRequireSentence

private class CoqAssertionSentenceImpl(
    private val sentence : Sentence,
    private val parent : ICoqElement with IParent)
        extends CoqScriptSentenceImpl(sentence, parent)
            with ICoqAssertionSentence

private class CoqSectionStartSentenceImpl(
    private val sentence : Sentence,
    private val parent : ICoqElement with IParent)
        extends CoqScriptSentenceImpl(sentence, parent)
            with ICoqSectionStartSentence

private class CoqModuleStartSentenceImpl(
    private val sentence : Sentence,
    private val parent : ICoqElement with IParent)
        extends CoqScriptSentenceImpl(sentence, parent)
            with ICoqModuleStartSentence

private class CoqScriptGroupImpl(
    val elements : Seq[ICoqScriptElement],
    val parent : ICoqElement with IParent)
    extends ParentImpl(None, parent) with ICoqScriptGroup {
  override def getChildren = elements

  override def toString = s"CoqScriptGroupImpl(${getDeterminingSentence})"
}

private case class CoqObjectFileImpl(
    private val res : Option[IFile],
    private val parent : ICoqPackageFragment)
    extends CoqElementImpl(res, parent) with ICoqObjectFile {
  override def getVernacFile = None
}

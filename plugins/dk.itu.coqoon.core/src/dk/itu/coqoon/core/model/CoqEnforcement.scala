/* CoqEnforcement.scala 
 * Support for scanning Coq model objects for Coqoon-specific issues
 * Copyright © 2016 Alexander Faithfull
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

object CoqEnforcement {
  case class Issue(val id : String, val offset : Int,
      val length : Int, val message : String, val severityHint : Severity)

  abstract sealed class Severity
  object Severity {
    /* If an Issue simply isn't worth mentioning, a CoqEnforcementContext can
     * return a severity of Suppressed to hide it altogether. */
    object Suppressed extends Severity
    object Information extends Severity
    object Warning extends Severity
    object Error extends Severity
  }
  trait Runner {
    def getIssues(i : ICoqElement) : Seq[Issue]
  }
  trait RunnerProvider {
    def makeRunner() : Runner
  }

  object IsolatedRequire extends RunnerProvider {
    final val ID = "enforcement/isolatedRequire"
    def apply(sentence : ICoqRequireSentence) = {
      val leadingWhitespace = sentence.getText.takeWhile(_.isWhitespace).size
      Issue(IsolatedRequire.ID,
          leadingWhitespace, sentence.getLength - leadingWhitespace,
          "Require sentences should only occur at the beginning of a file",
          Severity.Warning)
    }
    override def makeRunner = new Runner
    class Runner extends CoqEnforcement.Runner {
      private var requireDone = false
      override def getIssues(i : ICoqElement) = {
        // println(i)
        i match {
          case s : ICoqRequireSentence if requireDone =>
            Seq(IsolatedRequire(s))
          case s : ICoqRequireSentence => Seq()
          case s : ICoqScriptSentence if s.isSynthetic => Seq()
          case s : ICoqScriptElement =>
            println(s"Bailing out after $s")
            requireDone = true
            Seq()
          case _ => Seq()
        }
      }
    }
  }

  def check(f : ICoqVernacFile, context : CoqEnforcementContext) :
      Map[ICoqElement, Seq[(Issue, Severity)]] = {
    val runners = Seq(IsolatedRequire.makeRunner)

    var complaints : Map[ICoqElement, Seq[(Issue, Severity)]] = Map()
    f.accept(element => {
      val filtered = runners.flatMap(
          runner => runner.getIssues(element)).flatMap(issue => {
        val severity = context.getSeverity(issue)
        if (severity != Severity.Suppressed) {
          Some((issue, severity))
        } else None
      })
      if (!filtered.isEmpty)
        complaints += (element -> filtered)
      true
    })
    complaints
  }

  def createMarkers(
      f : ICoqVernacFile, results : Seq[(Issue, Severity)]) = {
    import dk.itu.coqoon.core.ManifestIdentifiers
    import org.eclipse.core.resources.IMarker
    import scala.collection.JavaConversions._
    for (f <- f.getCorrespondingResource;
         (Issue(_, offset, length, message, _), severity) <- results)
      f.createMarker(ManifestIdentifiers.MARKER_PROBLEM).setAttributes(Map(
          IMarker.MESSAGE -> message.trim,
          IMarker.SEVERITY -> (severity match {
            case Severity.Information => IMarker.SEVERITY_INFO
            case Severity.Warning => IMarker.SEVERITY_WARNING
            case Severity.Error => IMarker.SEVERITY_ERROR
            case _ => IMarker.SEVERITY_INFO
          }),
          IMarker.LOCATION -> s"offset $offset",
          IMarker.CHAR_START -> offset,
          IMarker.CHAR_END -> (offset + length),
          IMarker.TRANSIENT -> true))
  }
}

import CoqEnforcement.{Issue, Severity}

/* Some issues are big problems in some contexts (i.e., PIDE really doesn't
 * work well when there's a "Require Import Foo." sentence mid-way through a
 * document) but not problematic at all in others. */
trait CoqEnforcementContext {
  def getSeverity(issue : Issue) : Severity
}

object StandardEnforcementContext extends CoqEnforcementContext {
  override def getSeverity(issue : Issue) = issue.severityHint
}

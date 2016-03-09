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
  object Issue {
    def apply(id : String, sentence : ICoqScriptSentence,
        message : String, severityHint : Severity) : Issue = {
      val leadingWhitespace = sentence.getText.takeWhile(_.isWhitespace).size
      Issue(id, leadingWhitespace, sentence.getLength - leadingWhitespace,
          message, severityHint)
    }
  }

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
    def apply(sentence : ICoqRequireSentence) =
      Issue(IsolatedRequire.ID, sentence,
          "Require sentences should only occur at the beginning of a file",
          Severity.Warning)
    override def makeRunner = new Runner
    class Runner extends CoqEnforcement.Runner {
      private var requireDone = false
      override def getIssues(i : ICoqElement) = {
        i match {
          case s : ICoqRequireSentence if requireDone =>
            Seq(IsolatedRequire(s))
          case s : ICoqRequireSentence => Seq()
          case s : ICoqScriptSentence if s.isSynthetic => Seq()
          case s : ICoqScriptElement =>
            requireDone = true
            Seq()
          case _ => Seq()
        }
      }
    }
  }

  def check(f : ICoqVernacFile, context : CoqEnforcementContext) :
      Map[ICoqElement, Map[Issue, Severity]] = {
    val runners = Seq(IsolatedRequire.makeRunner)

    var complaints : Map[ICoqElement, Map[Issue, Severity]] = Map()
    f.accept(element => {
      val filtered = runners.flatMap(
          runner => runner.getIssues(element)).flatMap(issue => {
        val severity = context.getSeverity(issue)
        if (severity != Severity.Suppressed) {
          Some((issue, severity))
        } else None
      })
      if (!filtered.isEmpty)
        complaints += (element -> filtered.toMap)
      true
    })
    complaints
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

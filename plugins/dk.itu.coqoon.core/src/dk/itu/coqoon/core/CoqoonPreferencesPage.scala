/* CoqoonPreferencesPage.scala
 * Support for configuring Coqoon through the Eclipse preferences system
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

package dk.itu.coqoon.core

import dk.itu.coqoon.core.coqtop.CoqProgram

import org.eclipse.ui.{IWorkbench, IWorkbenchPreferencePage}
import org.eclipse.jface.preference.{FieldEditorPreferencePage => FEPP}

class CoqoonPreferencesPage
    extends FEPP(FEPP.GRID) with IWorkbenchPreferencePage {
  override def init(w : IWorkbench) =
    setPreferenceStore(Activator.getDefault.getPreferenceStore)

  import org.eclipse.jface.preference.DirectoryFieldEditor
  override def createFieldEditors =
    addField(new DirectoryFieldEditor(
        "coqpath", "Path to Coq", getFieldEditorParent))

  override def performOk = {
    super.performOk()
    if (!CoqProgram("coqtop").check) {
      setErrorMessage("Can't find the coqtop program")
      false
    } else true
  }
}

object CoqoonPreferences {
  import org.eclipse.core.runtime.{Path, IPath}
  def getCoqPath() : Option[IPath] =
    Option(Activator.getDefault.getPreferenceStore.getString("coqpath")).
        map(_.trim).filter(_.length != 0).map(p => new Path(p))
}
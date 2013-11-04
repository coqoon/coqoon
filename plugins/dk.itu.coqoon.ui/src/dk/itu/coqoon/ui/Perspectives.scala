/* Perspectives.scala
 * Perspective layout definitions
 * Copyright © 2013 Alexander Faithfull
 *
 * You may use, copy, modify and/or redistribute this code subject to the terms
 * of either the license of Kopitiam or the Apache License, version 2.0 */

package dk.itu.coqoon.ui

import org.eclipse.ui.IPerspectiveFactory

class CoqPerspectiveFactory extends IPerspectiveFactory {
  import org.eclipse.ui.IPageLayout
  import org.eclipse.ui.console.IConsoleConstants

  def createInitialLayout(layout: IPageLayout): Unit = {
    val editorA = layout.getEditorArea()
    val leftA = layout.createFolder(
        "leftA", IPageLayout.LEFT, 0.20f, editorA)
    val bottomA = layout.createFolder(
        "bottomA", IPageLayout.BOTTOM, 0.80f, editorA)
    val rightA = layout.createFolder(
        "rightA", IPageLayout.RIGHT, 0.75f, editorA)

    leftA.addView(IPageLayout.ID_PROJECT_EXPLORER)

    rightA.addView(ManifestIdentifiers.VIEW_GOAL_VIEWER)
    rightA.addView(IPageLayout.ID_OUTLINE)

    bottomA.addView(IConsoleConstants.ID_CONSOLE_VIEW)
    bottomA.addView(IPageLayout.ID_PROBLEM_VIEW)
  }
}

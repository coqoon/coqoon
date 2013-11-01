package dk.itu.coqoon.ui

import dk.itu.sdg.kopitiam

object Activator {
  def getDefault() = kopitiam.Activator.getDefault
}

object ManifestIdentifiers {
  final val PLUGIN = kopitiam.ManifestIdentifiers.PLUGIN
  final val ANNOTATION_PROCESSED = "dk.itu.sdg.kopitiam.processed"
  final val ANNOTATION_PROCESSING = "dk.itu.sdg.kopitiam.processing"
  final val VIEW_GOAL_VIEWER = "kopitiam.GoalViewer"
  final val COMMAND_TOGGLE_COQ_FLAG = "Kopitiam.toggle_coq_flag"
  final val COMMAND_PARAMETER_TOGGLE_COQ_FLAG_NAME =
    "Kopitiam.toggle_coq_flag.name"
}

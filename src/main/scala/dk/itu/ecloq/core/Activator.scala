package dk.itu.ecloq.core

import dk.itu.sdg.kopitiam

object Activator {
  def getDefault() = kopitiam.Activator.getDefault
}

object ManifestIdentifiers {
  final val PLUGIN = kopitiam.ManifestIdentifiers.PLUGIN
  final val BUILDER_COQ = "dk.itu.sdg.kopitiam.CoqBuilder"
  final val CONTENT_TYPE_COQFILE = "dk.itu.sdg.kopitiam.CoqFile"
  final val CONTENT_TYPE_COQOBJECTFILE = "dk.itu.sdg.kopitiam.CoqObjectFile"
  final val MARKER_PROBLEM = "dk.itu.sdg.kopitiam.problemmarker"
  final val NATURE_COQ = "dk.itu.sdg.kopitiam.CoqNature"
}

package dk.itu.sdg.analysis

import  dk.itu.sdg.analysis.Purity._

object RegExpGenerator {

  def regularExpressions(analysisResult: Result): List[String] =
    NFA.fromPurityResult(analysisResult).allPaths

  def print(analysisResult: Result): Unit =
    NFA.fromPurityResult(analysisResult).print

}
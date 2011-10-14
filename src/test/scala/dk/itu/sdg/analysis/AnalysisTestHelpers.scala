package dk.itu.sdg.analysis

import dk.itu.sdg.javaparser._

object AnalysisTestHelpers {
  
  def methodsOf(id: String, ast: List[SJDefinition]) = 
    ast.filter(_.id == id)
       .head.body
       .filter(_.isInstanceOf[SJMethodDefinition])
       .map(_.asInstanceOf[SJMethodDefinition])
  
  def constructorOf(id: String, ast: List[SJDefinition]) = 
    ast.filter(_.id == id)
       .head.body
       .filter(_.isInstanceOf[SJConstructorDefinition])
       .map(_.asInstanceOf[SJConstructorDefinition])
       .head 
}
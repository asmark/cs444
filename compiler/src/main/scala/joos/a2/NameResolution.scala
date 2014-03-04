package joos.a2

import joos.ast.AbstractSyntaxTree
import joos.ast.declarations.ModuleDeclaration
import joos.semantic.names.environments.EnvironmentBuilder
import joos.semantic.names.{TypeLinker, SimpleHierarchyAnalyzer, AdvancedHierarchyAnalyzer}

object NameResolution {

  def getAnalyzers = {
    implicit val module = new ModuleDeclaration
    Seq(
      new EnvironmentBuilder,
      new TypeLinker,
      new SimpleHierarchyAnalyzer,
      new AdvancedHierarchyAnalyzer
    )
  }

  def apply(asts: Seq[AbstractSyntaxTree]) {
    getAnalyzers foreach {
      analyzer =>
        asts foreach (_.dispatch(analyzer))
    }
  }
}

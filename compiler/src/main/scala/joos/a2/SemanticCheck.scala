package joos.a2

import joos.analyzers.{AdvancedHierarchyAnalyzer, SimpleHierarchyAnalyzer, EnvironmentLinker, TypeEnvironmentBuilder}
import joos.ast.AbstractSyntaxTree
import joos.ast.declarations.ModuleDeclaration

object SemanticCheck {

  def getAnalyzers = {
    implicit val module = new ModuleDeclaration
    Seq(
      new EnvironmentLinker,
      new TypeEnvironmentBuilder,
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

package joos.a2

import joos.ast.AbstractSyntaxTree
import joos.ast.declarations.ModuleDeclaration
import joos.semantic.names.heirarchy.{SimpleHierarchyChecker, AdvancedHierarchyChecker}
import joos.semantic.names.link.TypeLinker
import joos.semantic.names.environment.EnvironmentBuilder

object NameResolution {

  def getAnalyzers = {
    implicit val module = new ModuleDeclaration
    Seq(
      new EnvironmentBuilder,
      new TypeLinker,
      new SimpleHierarchyChecker,
      new AdvancedHierarchyChecker
    )
  }

  def apply(asts: Seq[AbstractSyntaxTree]) {
    getAnalyzers foreach {
      analyzer =>
        asts foreach (_.dispatch(analyzer))
    }
  }
}

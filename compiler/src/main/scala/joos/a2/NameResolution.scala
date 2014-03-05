package joos.a2

import joos.ast.AbstractSyntaxTree
import joos.ast.declarations.ModuleDeclaration
import joos.semantic.names.environment.EnvironmentBuilder
import joos.semantic.names.heirarchy.{SimpleHierarchyChecker, AdvancedHierarchyChecker}
import joos.semantic.names.link.TypeLinker

object NameResolution {

  def getAnalyzers(ast: AbstractSyntaxTree)(implicit module: ModuleDeclaration) = {
    implicit val unit = ast.root
    Seq(
      new EnvironmentBuilder,
      new TypeLinker,
      new SimpleHierarchyChecker,
      new AdvancedHierarchyChecker
    )
  }


  def apply(asts: Seq[AbstractSyntaxTree]) {
    implicit val module = new ModuleDeclaration

    val analyzers = 4
    for (i <- Range(0, analyzers)) {
      for (ast <- asts) {
        ast dispatch getAnalyzers(ast).apply(i)
      }
    }
  }

}

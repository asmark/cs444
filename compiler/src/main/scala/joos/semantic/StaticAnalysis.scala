package joos.semantic

import joos.ast.AbstractSyntaxTree
import joos.ast.declarations.ModuleDeclaration
import joos.semantic.names.heirarchy.AdvancedHierarchyChecker
import joos.semantic.reachability.ReachabilityChecker

object StaticAnalysis {

  def getAnalyzers(ast: AbstractSyntaxTree)(implicit module: ModuleDeclaration) = {
    implicit val unit = ast.root
    Seq(
      new ReachabilityChecker
    )
  }


  def apply(asts: Seq[AbstractSyntaxTree]) {
    implicit val module = new ModuleDeclaration

    val analyzers = 1
    for (i <- Range(0, analyzers)) {
      for (ast <- asts) {
        ast dispatch getAnalyzers(ast).apply(i)
      }
    }
  }

}

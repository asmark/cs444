package joos.semantic

import joos.ast.AbstractSyntaxTree
import joos.ast.declarations.ModuleDeclaration
import joos.semantic.types.disambiguation.StaticDisambiguationChecker

object TypeChecking {

  def getAnalyzers(ast: AbstractSyntaxTree)(implicit module: ModuleDeclaration) = {
    implicit val unit = ast.root
    Seq(
      new StaticDisambiguationChecker
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

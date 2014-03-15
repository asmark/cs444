package joos.semantic

import joos.ast.AbstractSyntaxTree
import joos.ast.declarations.ModuleDeclaration
import joos.semantic.types.disambiguation.{StaticNameLinker, NameDisambiguator, NameClassifier}
import joos.semantic.types.checking.TypeChecker

object TypeChecking {

  def getAnalyzers(ast: AbstractSyntaxTree)(implicit module: ModuleDeclaration) = {
    implicit val unit = ast.root
    Seq(
      new NameClassifier,
      new NameDisambiguator,
      //      new StaticAndVariableNameLinker,
      new StaticNameLinker,
//      new SubExpressionLifter
            new TypeChecker
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

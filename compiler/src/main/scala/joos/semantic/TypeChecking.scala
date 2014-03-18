package joos.semantic

import joos.ast.AbstractSyntaxTree
import joos.ast.declarations.ModuleDeclaration
import joos.semantic.types.checking.TypeChecker
import joos.semantic.types.disambiguation.{DeclarationReferenceLinker, StaticNameLinker, NameDisambiguator, NameClassifier}

object TypeChecking {

  private[this] final val analyzerBuilders = List(
    DeclarationReferenceLinker,
    NameClassifier,
    NameDisambiguator,
    StaticNameLinker,
    TypeChecker
  )

  def apply(asts: Seq[AbstractSyntaxTree]) {
    implicit val module = new ModuleDeclaration

    for (builder <- analyzerBuilders) {
      for (ast <- asts) {
        implicit val unit = ast.root
        ast dispatch builder.build
      }
    }
  }
}

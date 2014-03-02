package joos.a2

import joos.analyzers.{EnvironmentLinker, TypeEnvironmentBuilder}
import joos.ast.AbstractSyntaxTree
import joos.ast.declarations.ModuleDeclaration

object SemanticCheck {

  def getAnalyzers = {
    implicit val module = new ModuleDeclaration
    Seq(new EnvironmentLinker, new TypeEnvironmentBuilder)
  }

  def apply(asts: Seq[AbstractSyntaxTree]) {
    getAnalyzers foreach {
      analyzer =>
        asts foreach (_.dispatch(analyzer))
    }
  }
}

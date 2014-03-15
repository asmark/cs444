package joos.semantic.types.checking

import joos.ast.visitor.AstVisitor
import joos.ast.declarations.VariableDeclarationFragment

trait VariableDeclarationFragmentTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(variableDeclarationFragment: VariableDeclarationFragment) {
    variableDeclarationFragment.initializer match {
      case Some(initializer) => {
        initializer.accept(this)
        require(variableDeclarationFragment.initializer.get.declarationType != null)
      }
      case _ =>
    }
  }
}

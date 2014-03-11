package joos.semantic.types.disambiguation

import joos.ast.visitor.AstCompleteVisitor
import joos.ast.declarations.{VariableDeclarationFragment, FieldDeclaration, SingleVariableDeclaration}
import joos.ast.expressions._
import joos.ast.CompilationUnit

class ForwardFieldChecker(fields: Map[SimpleNameExpression, FieldDeclaration])(implicit unit: CompilationUnit) extends AstCompleteVisitor {

  override def apply(fragment: VariableDeclarationFragment) {
    // Do not visit fragment.identifier because we have not put that in the map yet
    fragment.initializer foreach (_.accept(this))
  }

  override def apply(name: SimpleNameExpression) {
    if (fields.get(name).isEmpty) {
      throw new ForwardFieldUseException(name)
    }
  }

}
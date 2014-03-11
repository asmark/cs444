package joos.semantic.types.disambiguation

import joos.ast.visitor.AstCompleteVisitor
import joos.ast.declarations.{FieldDeclaration, SingleVariableDeclaration}
import joos.ast.expressions._
import joos.ast.CompilationUnit

class ForwardFieldChecker(fields: Map[SimpleNameExpression, FieldDeclaration])(implicit unit: CompilationUnit) extends AstCompleteVisitor {

  override def apply(name: SimpleNameExpression) {
    if (fields.get(name).isEmpty) {
      throw new ForwardFieldUseException(name)
    }
  }

}
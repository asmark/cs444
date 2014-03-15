package joos.semantic.types.checking

import joos.ast.expressions.ThisExpression
import joos.ast.visitor.AstVisitor
import joos.semantic.types.ImplicitThisInStaticException

trait ThisExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>

  override def apply(thisExpression: ThisExpression) {
    if (checkImplicitThis)
      throw new ImplicitThisInStaticException("")

    thisExpression.declarationType = unit.typeDeclaration.get.asType
  }
}

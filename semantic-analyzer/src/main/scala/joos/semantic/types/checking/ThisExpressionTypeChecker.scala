package joos.semantic.types.checking

import joos.ast.expressions.ThisExpression
import joos.ast.visitor.AstVisitor
import joos.semantic.types.ExplicitThisInStaticException

trait ThisExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>

  override def apply(thisExpression: ThisExpression) {
    if (checkExplicitThis)
      throw new ExplicitThisInStaticException("Found an explicit this call in a static method or field")

    thisExpression.declarationType = unit.typeDeclaration.get.asType
  }
}

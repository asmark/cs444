package joos.semantic.types.checking

import joos.ast.expressions.ArrayCreationExpression
import joos.ast.visitor.AstVisitor
import joos.semantic.types.ArrayCreationException
import joos.ast.types.ArrayType

trait ArrayCreationExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>

  override def apply(expression: ArrayCreationExpression) {
    expression.size.accept(this)

    require(expression.size.declarationType != null)

    if (!expression.size.declarationType.isNumeric) {
      throw new ArrayCreationException(s"Invalid size for array creation: ${expression.size.declarationType.standardName}")
    }

    expression.declarationType = ArrayType(expression.arrayType)
  }
}

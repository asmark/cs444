package joos.semantic.types.checking

import joos.ast.expressions.ArrayCreationExpression
import joos.ast.visitor.AstVisitor
import joos.semantic.types.ArrayCreationException
import joos.ast.types.ArrayType

trait ArrayCreationExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>

  override def apply(expression: ArrayCreationExpression) {
    expression.size.accept(this)

    require(expression.size.expressionType != null)

    if (!expression.size.expressionType.isNumeric) {
      throw new ArrayCreationException(s"Invalid size for array creation: ${expression.size.expressionType.standardName}")
    }

    expression.expressionType = ArrayType(expression.arrayType)
  }
}

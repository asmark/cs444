package joos.semantic.types.checking

import joos.ast.expressions.ArrayCreationExpression
import joos.ast.types.{ArrayType, PrimitiveType}
import joos.syntax.tokens.{TokenKind, TerminalToken}
import joos.semantic.types.ArrayCreationException
import joos.ast.visitor.AstVisitor

trait ArrayCreationExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(arrayCreationExpression: ArrayCreationExpression) {
    arrayCreationExpression.size.accept(this)

    require(arrayCreationExpression.size.declarationType != null)
    if (PrimitiveType.isNumeric(arrayCreationExpression.size.declarationType)) {
      arrayCreationExpression.declarationType = ArrayType(arrayCreationExpression.arrayType)
    } else {
      throw new ArrayCreationException(s"invalid size expression in array creation: ${arrayCreationExpression.arrayType.standardName}")
    }
  }
}

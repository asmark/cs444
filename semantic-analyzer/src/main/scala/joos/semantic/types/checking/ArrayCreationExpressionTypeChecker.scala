package joos.semantic.types.checking

import joos.ast.expressions.ArrayCreationExpression
import joos.ast.types.{ArrayType, PrimitiveType}
import joos.syntax.tokens.{TokenKind, TerminalToken}
import joos.semantic.types.ArrayCreationException

trait ArrayCreationExpressionTypeChecker {
  self: TypeChecker =>
  override def apply(arrayCreationExpression: ArrayCreationExpression) {
    arrayCreationExpression.size.accept(this)
    arrayCreationExpression.size.declarationType match {
      case PrimitiveType(TerminalToken(integer, TokenKind.DecimalIntLiteral)) =>
        arrayCreationExpression.declarationType = ArrayType(arrayCreationExpression.arrayType, integer.toInt)
      case _ => throw new ArrayCreationException(s"invalid size expression in array creation: ${arrayCreationExpression.arrayType.standardName}")
    }
  }
}

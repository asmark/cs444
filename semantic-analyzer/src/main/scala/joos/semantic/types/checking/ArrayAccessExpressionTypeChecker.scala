package joos.semantic.types.checking

import joos.ast.expressions.ArrayAccessExpression
import joos.ast.types.{PrimitiveType, ArrayType}
import joos.syntax.tokens.{TokenKind, TerminalToken}
import joos.semantic.types.ArrayAccessException

trait ArrayAccessExpressionTypeChecker {
  self: TypeChecker =>
  override def apply(arrayAccessExpression: ArrayAccessExpression) {
    arrayAccessExpression.reference.accept(this)
    arrayAccessExpression.index.accept(this)

    arrayAccessExpression.reference.declarationType match {
      case arrayType: ArrayType => {
        arrayAccessExpression.index.declarationType match {
          case PrimitiveType(TerminalToken(_, TokenKind.DecimalIntLiteral)) =>
            arrayAccessExpression.declarationType = arrayType.elementType
          case _ => throw new ArrayAccessException(
            s"invalid index type ${arrayAccessExpression.index.declarationType.standardName} in ${arrayType.elementType}[]"
          )
        }
      }
      case _ => throw new ArrayAccessException(s"invalid reference type ${arrayAccessExpression.reference.declarationType.standardName}")
    }
  }
}

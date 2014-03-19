package joos.semantic.types.checking

import joos.ast.expressions.{ArrayCreationExpression, ArrayAccessExpression}
import joos.ast.types.{Type, PrimitiveType, ArrayType}
import joos.syntax.tokens.{TokenKind, TerminalToken}
import joos.semantic.types.ArrayAccessException
import joos.ast.visitor.AstVisitor

trait ArrayAccessExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(arrayAccessExpression: ArrayAccessExpression) {
    arrayAccessExpression.reference.accept(this)
    arrayAccessExpression.index.accept(this)

    require(arrayAccessExpression.reference.expressionType != null)
    arrayAccessExpression.reference.expressionType match {
      case arrayType: ArrayType => {
        require(arrayAccessExpression.index.expressionType != null)
        if (arrayAccessExpression.index.expressionType.isNumeric) {
          arrayAccessExpression.expressionType = arrayType.elementType
        } else {
          throw new ArrayAccessException(
            s"invalid index type ${arrayAccessExpression.index.expressionType.standardName} in ${arrayType.elementType}[]"
          )
        }
      }
      case _ => throw new ArrayAccessException(s"invalid reference type ${arrayAccessExpression.reference.expressionType.standardName}")
    }
  }
}

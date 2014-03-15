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

    require(arrayAccessExpression.reference.declarationType != null)
    arrayAccessExpression.reference.declarationType match {
      case arrayType: ArrayType => {
        // TODO: the index type undergoes promotion
        require(arrayAccessExpression.index.declarationType != null)
        if (arrayAccessExpression.index.declarationType.isNumeric) {
          arrayAccessExpression.declarationType = arrayType.elementType
        } else {
          throw new ArrayAccessException(
            s"invalid index type ${arrayAccessExpression.index.declarationType.standardName} in ${arrayType.elementType}[]"
          )
        }
      }
      case _ => throw new ArrayAccessException(s"invalid reference type ${arrayAccessExpression.reference.declarationType.standardName}")
    }
  }
}

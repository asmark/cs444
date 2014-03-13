package joos.semantic.types.checking

import joos.ast.expressions.{ArrayCreationExpression, ArrayAccessExpression}
import joos.ast.types.{PrimitiveType, ArrayType}
import joos.syntax.tokens.{TokenKind, TerminalToken}
import joos.semantic.types.ArrayAccessException
import joos.ast.visitor.AstVisitor

trait ArrayAccessExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(arrayAccessExpression: ArrayAccessExpression) {
    arrayAccessExpression.reference.accept(this)
    arrayAccessExpression.index.accept(this)

    arrayAccessExpression.reference.declarationType match {
      case arrayType: ArrayType => {
        // TODO: the index type undergoes promotion
        if (PrimitiveType.isNumeric(arrayAccessExpression.index.declarationType)) {
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

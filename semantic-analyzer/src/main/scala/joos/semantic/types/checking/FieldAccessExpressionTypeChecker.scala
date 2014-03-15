package joos.semantic.types.checking

import joos.ast.expressions.FieldAccessExpression
import joos.ast.types.{ArrayType, PrimitiveType, SimpleType}
import joos.ast.visitor.AstVisitor
import joos.semantic.types.FieldAccessExpressionException

trait FieldAccessExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>

  override def apply(fieldAccessExpression: FieldAccessExpression) {
    fieldAccessExpression.expression.accept(this)
    require(fieldAccessExpression.expression.declarationType != null)
    val prefixType = fieldAccessExpression.expression.declarationType
    val fieldName = fieldAccessExpression.identifier

    /*
    * If the identifier does not name an accessible member field of type T,
    * then the field access is undefined and a compile-time error occurs.
    */
    fieldAccessExpression.declarationType = prefixType match {
      case _: PrimitiveType | ArrayType =>
        throw new FieldAccessExpressionException(s"Primary expression is not of a reference type ${prefixType.standardName}")
      case prefixType: SimpleType => {
        prefixType.declaration.get.containedFields.get(fieldName) match {
          case None => throw new FieldAccessExpressionException(s"Cannot locate field ${fieldName}")
          case Some(declaration) => declaration.variableType
        }
      }
    }
  }
}

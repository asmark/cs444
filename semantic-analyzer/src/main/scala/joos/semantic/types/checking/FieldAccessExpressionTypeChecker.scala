package joos.semantic.types.checking

import joos.ast.visitor.AstVisitor
import joos.ast.expressions.FieldAccessExpression
import joos.ast.types.SimpleType
import joos.semantic.types.FieldAccessExpressionException
import joos.semantic._

trait FieldAccessExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(fieldAccessExpression: FieldAccessExpression) {
    fieldAccessExpression.expression.accept(this)
    require(fieldAccessExpression.expression.declarationType != null)
    val primaryType = fieldAccessExpression.expression.declarationType

    fieldAccessExpression.identifier.accept(this)
    require(fieldAccessExpression.identifier.declarationType != null)
    val identifierType = fieldAccessExpression.identifier.declarationType

    if (!primaryType.isInstanceOf[SimpleType]) {
      throw new FieldAccessExpressionException(s"Primary expression is not of a reference type ${primaryType.standardName}")
    }

    /*
    * If the identifier does not name an accessible member field of type T,
    * then the field access is undefined and a compile-time error occurs.
    */
    primaryType match {
      case SimpleType(typeName) => {
        unit.getVisibleType(typeName) match {
          case Some(typeDeclaration) => {
            // TODO: How to avoid checking the same declaration again?
            typeDeclaration.accept(this)

            val containedFields = typeDeclaration.containedFields
            val fieldName = fieldAccessExpression.identifier
            val option = containedFields.get(fieldName)
            if (!option.isDefined) {
              throw new FieldAccessExpressionException(s"Unable to find field ${fieldName.standardName} of type ${typeName}")
            }

            val fieldDeclaration = option.get
            require(fieldDeclaration.declarationType != null)
            if (fieldDeclaration.declarationType equals null) {
              fieldDeclaration.accept(this)
            }

            if (!areEqual(fieldDeclaration.declarationType, identifierType)) {
              throw new FieldAccessExpressionException(s"Unable to find field ${fieldName.standardName} of type ${typeName}")
            }
          }
          case None =>
            throw new FieldAccessExpressionException(s"Unable to find type ${typeName}")
        }
      }
      case _ =>
        throw new FieldAccessExpressionException(s"Primary expression is not of a reference type ${primaryType.standardName}")
    }

    fieldAccessExpression.declarationType = identifierType
  }
}

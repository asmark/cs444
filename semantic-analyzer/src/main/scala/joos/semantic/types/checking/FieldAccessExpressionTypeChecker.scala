package joos.semantic.types.checking

import joos.ast.expressions.FieldAccessExpression
import joos.ast.types.{ArrayType, PrimitiveType, SimpleType}
import joos.ast.visitor.AstVisitor
import joos.semantic.types.{IllegalProtectedFieldAccessException, FieldAccessExpressionException}
import joos.ast.Modifier

trait FieldAccessExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>

  override def apply(fieldAccessExpression: FieldAccessExpression) {
    fieldAccessExpression.expression.accept(this)
    require(fieldAccessExpression.expression.declarationType != null)
    val prefixType = fieldAccessExpression.expression.declarationType
    val fieldName = fieldAccessExpression.identifier

//    if (fieldName.standardName.contains("bar")) {
//      println("bar")
//    }

    /*
    * If the identifier does not name an accessible member field of type T,
    * then the field access is undefined and a compile-time error occurs.
    */
    fieldAccessExpression.declarationType = prefixType match {
      case _: PrimitiveType =>
        throw new FieldAccessExpressionException(s"field ${fieldName} does not exist in ${prefixType.standardName}")
      case _: ArrayType =>
        if (fieldName.standardName == "length") PrimitiveType.IntegerType
        else throw new FieldAccessExpressionException(s"field ${fieldName} does not exist in ${prefixType.standardName}")
      case prefixType: SimpleType => {
        prefixType.declaration.get.containedFields.get(fieldName) match {
          case None => throw new FieldAccessExpressionException(s"field ${fieldName} does not exist in ${prefixType.standardName}")
          case Some(declaration) => {
//            println(declaration.fragment.identifier.standardName)
//            if (declaration.fragment.identifier.standardName.contains("bar")) {
//              println("bar")
//            }
            if (declaration.modifiers.contains(Modifier.Protected) && declaration.typeDeclaration != unit.typeDeclaration.get) {
              // Check that all accesses of protected fields, methods and constructors are
              // in a subtype of the type declaring the entity being accessed, or in the same package as that type.
              if (!unit.typeDeclaration.get.allAncestors.contains(declaration.typeDeclaration) &&
                  unit.packageDeclaration != declaration.typeDeclaration.packageDeclaration) {
                throw new IllegalProtectedFieldAccessException(
                  s"${fieldAccessExpression.toString} pointing to ${declaration.declarationName.standardName}"
                )
              }
            }
            declaration.variableType
          }
        }
      }
    }
  }
}

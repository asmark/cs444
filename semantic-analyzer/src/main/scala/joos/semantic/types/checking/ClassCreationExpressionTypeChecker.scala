package joos.semantic.types.checking

import joos.ast.expressions.ClassCreationExpression
import joos.ast.types.SimpleType
import joos.ast.{AstNode, Modifier}
import joos.semantic.types.{ClassCreationException, AbstractClassCreationException}
import joos.ast.visitor.AstVisitor

trait ClassCreationExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(classCreationExpression: ClassCreationExpression) {
    // Check that no objects of abstract classes are created
    val classType = classCreationExpression.classType
    classType match {
      case SimpleType(className) =>
        self.unit.getVisibleType(className) match {
          case Some(typeDeclaration) => {
            if (typeDeclaration.modifiers.contains(Modifier.Abstract)) {
              throw new AbstractClassCreationException(s"Attempt to create class: ${typeDeclaration.name.standardName}")
            }
            classCreationExpression.declarationType = SimpleType(className)
          }
          case _ => throw new ClassCreationException(s"Attempt to create class: ${classType.standardName}")
        }
    }
  }
}

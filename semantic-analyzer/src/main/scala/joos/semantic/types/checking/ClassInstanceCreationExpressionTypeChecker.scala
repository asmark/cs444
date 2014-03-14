package joos.semantic.types.checking

import joos.ast.expressions.{NameExpression, ClassInstanceCreationExpression}
import joos.ast.types.SimpleType
import joos.ast.{Modifier}
import joos.semantic.types.{InterfaceInstanceCreationException, ClassInstanceCreationException, AbstractOrInstanceCreationException}
import joos.ast.visitor.AstVisitor
import joos.semantic._
import joos.ast.declarations.TypeDeclaration

trait ClassInstanceCreationExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(classInstanceCreationExpression: ClassInstanceCreationExpression) {
    classInstanceCreationExpression.arguments.foreach(
      expr => {
        expr.accept(this)
        require(expr.declarationType != null)
      }
    )

    // Check that no objects of abstract classes are created
    val classType = classInstanceCreationExpression.classType

    classType match {
      case SimpleType(className) =>
        self.unit.getVisibleType(className) match {
          case Some(typeDeclaration) => {
            if (!typeDeclaration.isConcreteClass) {
              throw new AbstractOrInstanceCreationException(s"Attempt to create abstract class instance: ${typeDeclaration.name.standardName}")
            }

            classInstanceCreationExpression.declarationType = SimpleType(NameExpression(fullName(typeDeclaration)))
          }
          case _ => throw new ClassInstanceCreationException(s"Attempt to create class: ${classType.standardName}")
        }
    }
  }
}

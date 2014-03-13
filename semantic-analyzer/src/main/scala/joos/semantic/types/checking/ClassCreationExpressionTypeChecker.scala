package joos.semantic.types.checking

import joos.ast.expressions.{NameExpression, ClassInstanceCreationExpression}
import joos.ast.types.SimpleType
import joos.ast.{Modifier}
import joos.semantic.types.{ClassCreationException, AbstractClassCreationException}
import joos.ast.visitor.AstVisitor
import joos.semantic._

trait ClassCreationExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(classCreationExpression: ClassInstanceCreationExpression) {
    classCreationExpression.arguments.foreach(
      expr => {
        expr.accept(this)
        require(expr.declarationType != null)
      }
    )

    // Check that no objects of abstract classes are created
    val classType = classCreationExpression.classType

    classType match {
      case SimpleType(className) =>
        self.unit.getVisibleType(className) match {
          case Some(typeDeclaration) => {
            if (typeDeclaration.modifiers.contains(Modifier.Abstract)) {
              throw new AbstractClassCreationException(s"Attempt to create class: ${typeDeclaration.name.standardName}")
            }
            classCreationExpression.declarationType = SimpleType(NameExpression(fullName(typeDeclaration)))
          }
          case _ => throw new ClassCreationException(s"Attempt to create class: ${classType.standardName}")
        }
    }
  }
}

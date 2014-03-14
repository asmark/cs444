package joos.semantic.types.checking

import joos.ast.expressions.{NameExpression, ClassInstanceCreationExpression}
import joos.ast.types.SimpleType
import joos.ast.visitor.AstVisitor
import joos.semantic.types.{ClassInstanceCreationException, AbstractOrInstanceCreationException}

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
        val typeDeclaration = unit.getVisibleType(className).get
        if (typeDeclaration.isConcreteClass) {
          throw new AbstractOrInstanceCreationException(s"Attempt to create abstract class instance: ${typeDeclaration.name.standardName}")
        }
        classInstanceCreationExpression.declarationType = SimpleType(NameExpression(typeDeclaration.fullName))
      case _ => throw new ClassInstanceCreationException(s"Attempt to create class: ${classType.standardName}")
    }
  }
}

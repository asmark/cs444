package joos.semantic.types.checking

import joos.ast.expressions.{NameExpression, ClassInstanceCreationExpression}
import joos.ast.types.SimpleType
import joos.ast.{Modifier}
import joos.semantic.types.{ClassInstanceCreationException, AbstractClassCreationException}
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
            if (typeDeclaration.modifiers.contains(Modifier.Abstract)) {
              throw new AbstractClassCreationException(s"Attempt to create class: ${typeDeclaration.name.standardName}")
            }

            classInstanceCreationExpression.declarationType = SimpleType(NameExpression(fullName(typeDeclaration)))
          }
          case _ => throw new ClassInstanceCreationException(s"Attempt to create class: ${classType.standardName}")
        }
    }
  }

//  private def checkProtectedConstructor(classInstanceCreationExpression: ClassInstanceCreationExpression, typeDeclaration: TypeDeclaration) {
//    val targetConstructor = typeDeclaration.constructorMap.values.find(
//      constructor => {
//        val params = constructor.parameters.toArray
//        val arguments = classInstanceCreationExpression.arguments.toArray
//        for (i <- 1 until params.length) {
//          if (params(i) != arguments(i))
//        }
//        return true
//      }
//    )
//  }
}

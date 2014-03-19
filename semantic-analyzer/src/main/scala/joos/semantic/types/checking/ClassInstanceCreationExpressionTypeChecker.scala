package joos.semantic.types.checking

import joos.ast.Modifier
import joos.ast.expressions.ClassInstanceCreationExpression
import joos.ast.types.SimpleType
import joos.ast.visitor.AstVisitor
import joos.semantic.types.TypeCheckingException
import joos.semantic.types.disambiguation._

trait ClassInstanceCreationExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>

  override def apply(newExpression: ClassInstanceCreationExpression) {
    newExpression.arguments.foreach {
      argument =>
        argument.accept(this)
        require(argument.expressionType != null)
    }

    val classType = newExpression.classType

    newExpression.expressionType = classType match {
      case classType: SimpleType =>
        val declaration = classType.declaration
        if (!declaration.isConcreteClass) {
          throw new TypeCheckingException("new", s"${classType.standardName} is not concrete")
        }

        findMethod(newExpression.arguments, declaration.constructorMap.values) match {
          case None => throw new TypeCheckingException("new", s"Cannot find constructor ${classType.name}")
          case Some(constructor) => {
            if (constructor.modifiers.contains(Modifier.Protected) &&
                unit.packageDeclaration != constructor.typeDeclaration.packageDeclaration)
              throw new TypeCheckingException("new", s"Cannot access protected constructor ${constructor.toString}")

            classType
          }
        }
      case _ => throw new TypeCheckingException("new", s"Cannot instantiate class ${classType.standardName}")
    }
  }
}

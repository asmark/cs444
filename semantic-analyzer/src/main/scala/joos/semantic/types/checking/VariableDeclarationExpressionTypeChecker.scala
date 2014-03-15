package joos.semantic.types.checking

import joos.ast.expressions.VariableDeclarationExpression
import joos.semantic._
import joos.semantic.types.{AstEnvironmentVisitor, TypeCheckingException}

trait VariableDeclarationExpressionTypeChecker extends AstEnvironmentVisitor {
  self: TypeChecker =>

  override def apply(variable: VariableDeclarationExpression) {
    super.apply(variable)

    variable.fragment.initializer match {
      case None =>
      case Some(initializer) =>
        initializer.accept(this)
        require(initializer.declarationType != null)
        initializer.accept(new InitializerTypeChecker(variable)(typeEnvironment, blockEnvironment))

        if (!isAssignable(variable.variableType, initializer.declarationType))
          throw new TypeCheckingException(
            "variable declaration",
            s"Cannot assign ${initializer.declarationType.standardName} to ${variable.declarationName}")
    }

    variable.declarationType = variable.variableType
  }
}

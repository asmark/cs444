package joos.semantic.types.checking

import joos.analysis.LocalVariableInitializerChecker
import joos.ast.expressions.{QualifiedNameExpression, VariableDeclarationExpression}
import joos.semantic._
import joos.semantic.types.{AstEnvironmentVisitor, TypeCheckingException}

trait VariableDeclarationExpressionTypeChecker extends AstEnvironmentVisitor {
  self: TypeChecker =>

  override def apply(name: QualifiedNameExpression) {
    name.qualifier.accept(this)
    name.name.accept(this)
  }

  override def apply(variable: VariableDeclarationExpression) {
    this.blockEnvironment = variable.blockEnvironment

    variable.fragment.initializer match {
      case None =>
        // Local variables must have initializer
        throw new TypeCheckingException("local variable declaration", "must have initializer")
      case Some(initializer) =>
        initializer.accept(this)
        require(initializer.expressionType != null)

        if (!isAssignable(variable.variableType, initializer.expressionType))
          throw new TypeCheckingException(
            "variable declaration",
            s"Cannot assign ${initializer.expressionType.standardName} to ${variable.declarationName}")

        new LocalVariableInitializerChecker(variable)(initializer)
    }

    variable.expressionType = variable.variableType
  }
}

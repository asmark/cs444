package joos.semantic.types.checking

import joos.ast.expressions.{QualifiedNameExpression, SimpleNameExpression, VariableDeclarationExpression}
import joos.semantic._
import joos.semantic.types.{AstEnvironmentVisitor, TypeCheckingException}

trait VariableDeclarationExpressionTypeChecker extends AstEnvironmentVisitor {
  self: TypeChecker =>

  private[this] var variable: VariableDeclarationExpression = null

  override def apply(name: SimpleNameExpression) {
    // Je_5_AmbiguousInvoke_LocalInOwnInitializer
    if (variable != null) {
      if (variable.declarationName == name) {
        throw new TypeCheckingException("initializer", s"Cannot use variable ${name} itself in its initializer")
      }
    }
  }

  override def apply(name: QualifiedNameExpression) {
    name.qualifier.accept(this)
    name.name.accept(this)
  }

  override def apply(variable: VariableDeclarationExpression) {
    this.variable = variable
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
    }

    variable.expressionType = variable.variableType

    this.variable = null
  }
}

package joos.semantic.types.checking

import joos.ast.expressions.{SimpleNameExpression, ParenthesizedExpression}
import joos.ast.visitor.AstVisitor
import joos.semantic.types.TypeCheckingException

trait ParenthesizedExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>

  override def apply(parenthesis: ParenthesizedExpression) {
    parenthesis.expression.accept(this)
    require(parenthesis.expression.declarationType != null)

    // Je_1_Dot_ParenthesizedType_Field
    parenthesis.expression match {
      case name: SimpleNameExpression =>
        // This has to refer to a local variable/instance field
        if (!blockEnvironment.contains(name) && !typeEnvironment.containedFields.contains(name)) {
          throw new TypeCheckingException("parenthesis", s"${name} needs to be a local variable or instance field")
        }
      case _ =>
    }

    parenthesis.declarationType = parenthesis.expression.declarationType
  }
}

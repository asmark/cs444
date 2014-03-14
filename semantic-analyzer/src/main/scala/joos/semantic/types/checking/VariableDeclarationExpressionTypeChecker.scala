package joos.semantic.types.checking

import joos.ast.visitor.AstVisitor
import joos.ast.expressions.VariableDeclarationExpression
import joos.semantic._
import joos.semantic.types.VariableDeclarationExpressionException

trait VariableDeclarationExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(variableDeclarationExpression: VariableDeclarationExpression) {
    variableDeclarationExpression.fragment.initializer match {
      // TODO: verify this is the only check needed
      case Some(initExpr) => {
        initExpr.accept(this)

        require(initExpr.declarationType != null)
        if(!isAssignable(variableDeclarationExpression.variableType, initExpr.declarationType))
          throw new VariableDeclarationExpressionException(
            s"attempt to assign ${initExpr.declarationType.standardName} to ${variableDeclarationExpression.variableType.standardName}"
          )

//        variableDeclarationExpression.declarationType = variableDeclarationExpression.variableType
      }
      case None => {
//        variableDeclarationExpression.declarationType = variableDeclarationExpression.variableType
      }
    }
  }
}

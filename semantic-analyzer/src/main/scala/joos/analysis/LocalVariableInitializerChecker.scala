package joos.analysis

import joos.ast.ExpressionDispatcher
import joos.ast.expressions.{SimpleNameExpression, VariableDeclarationExpression}
import joos.semantic.types.TypeCheckingException

class LocalVariableInitializerChecker(
    variable: VariableDeclarationExpression) extends ExpressionDispatcher {

  override def apply(name: SimpleNameExpression) {
    // Variable itself cannot occur in the initializer
    if (variable.declarationName == name)
      throw new TypeCheckingException("local variable initializer", s"${name} cannot occur in the initializer")
  }
}

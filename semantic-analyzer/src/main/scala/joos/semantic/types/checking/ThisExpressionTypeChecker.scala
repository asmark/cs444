package joos.semantic.types.checking

import joos.ast.visitor.AstVisitor
import joos.ast.expressions.ThisExpression
import joos.semantic.types.ImplicitThisInStaticException
import joos.ast.types.SimpleType

trait ThisExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(thisExpression: ThisExpression) {
    if (checkImplicitThis)
      throw new ImplicitThisInStaticException("")

    // TODO: The code below might be incorrect
    unit.typeDeclaration match {
      case Some(typeDeclaration) => thisExpression.declarationType = SimpleType(typeDeclaration.name)
      case _ => // TODO: exception ??
    }
  }
}

package joos.semantic.types.checking

import joos.ast.compositions.TypedDeclarationLike
import joos.ast.expressions.{QualifiedNameExpression, SimpleNameExpression}
import joos.ast.visitor.AstCompleteVisitor
import joos.semantic.types.TypeCheckingException
import joos.semantic.{TypeEnvironment, BlockEnvironment}

class InitializerTypeChecker(variable: TypedDeclarationLike)(
    implicit enclosingType: TypeEnvironment,
    enclosingBlock: BlockEnvironment) extends AstCompleteVisitor {

  override def apply(name: SimpleNameExpression) {
    // Je_5_AmbiguousInvoke_LocalInOwnInitializer
    if (variable.declarationName == name) {
      throw new TypeCheckingException("initializer", s"Cannot use variable ${name} itself in its initializer")
    }
  }

  override def apply(name: QualifiedNameExpression) {
    name.qualifier.accept(this)
    name.name.accept(this)
  }
}

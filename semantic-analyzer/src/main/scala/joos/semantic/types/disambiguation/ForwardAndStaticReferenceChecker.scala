package joos.semantic.types.disambiguation

import joos.ast.visitor.{AstVisitor, AstCompleteVisitor}
import joos.ast.{Modifier, CompilationUnit}
import joos.ast.types.Type
import joos.ast.declarations.{VariableDeclarationFragment, FieldDeclaration, TypeDeclaration}
import joos.ast.expressions._
import joos.semantic.TypeEnvironment

class ForwardAndStaticReferenceChecker(implicit unit: CompilationUnit) extends AstVisitor {
  private var context = Seq.empty[Modifier]
  private var typeEnvironment : TypeEnvironment= null

  override def apply(unit : CompilationUnit) {
    unit.typeDeclaration foreach (typeEnvironment = _)
    unit.typeDeclaration foreach (_.accept(this))
  }

  override def apply(typeDeclaration: TypeDeclaration) {
    typeDeclaration.fields foreach (_.accept(this))
    typeDeclaration.methods foreach (_.accept(this))
  }

  override def apply(fieldDeclaration: FieldDeclaration) {
    val oldContext = context
    if (fieldDeclaration.isStatic) {
      context = Seq(Modifier.Static)
    }
    fieldDeclaration.fragment.accept(this)
    context = oldContext
  }

  override def apply(fragment: VariableDeclarationFragment) {
    fragment.initializer foreach (_.accept(this))
  }

  override def apply(expression: ArrayAccessExpression) {
    expression.reference.accept(this)
    expression.index.accept(this)
  }

  override def apply(expression: ArrayCreationExpression) {
    expression.size.accept(this)
  }

  override def apply(expression: AssignmentExpression) {
    expression.left.accept(this)
    expression.right.accept(this)
  }

  override def apply(expression: CastExpression) {
    expression.expression.accept(this)
  }

  override def apply(expression: ClassCreationExpression) {
    expression.arguments foreach (_.accept(this))
  }

  override def apply(expression: FieldAccessExpression) {
    expression.expression.accept(this)
  }

  override def apply(expression: InfixExpression) {
    expression.left.accept(this)
    expression.right.accept(this)
  }

  override def apply(expression: InstanceOfExpression) {
    expression.expression.accept(this)
  }

  override def apply(expression: MethodInvocationExpression) {
    expression.expression foreach (_.accept(this))
    expression.arguments foreach (_.accept(this))
  }

  override def apply(expression: ParenthesizedExpression) {
    expression.expression.accept(this)
  }

  override def apply(expression: PrefixExpression) {
    expression.operand.accept(this)
  }

  override def apply(expression: VariableDeclarationExpression) {
    expression.declaration.accept(this)
  }

  override def apply(name: SimpleNameExpression) {

  }
}

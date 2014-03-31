package joos.ast

import joos.ast.expressions._

/**
 * Dispatches [[Expression]] to a more specific method
 */
trait ExpressionDispatcher {
  def apply(name: NameExpression) {
    name match {
      case name: QualifiedNameExpression => this(name)
      case name: SimpleNameExpression => this(name)
    }
  }

  def apply(literal: LiteralExpression) {
    literal match {
      case literal: BooleanLiteral => this(literal)
      case literal: CharacterLiteral => this(literal)
      case literal: IntegerLiteral => this(literal)
      case literal: NullLiteral => this(literal)
      case literal: StringLiteral => this(literal)
    }
  }

  protected def dispatchExpression(expression: Expression) = this(expression)

  def apply(expression: Expression) {
    expression match {
      case node: ArrayAccessExpression => this(node)
      case node: ArrayCreationExpression => this(node)
      case node: CastExpression => this(node)
      case node: ClassInstanceCreationExpression => this(node)
      case node: FieldAccessExpression => this(node)
      case node: InfixExpression => this(node)
      case node: InstanceOfExpression => this(node)
      case node: MethodInvocationExpression => this(node)
      case node: ParenthesizedExpression => this(node)
      case node: PrefixExpression => this(node)
      case name: NameExpression => this(name)
      case node: AssignmentExpression => this(node)
      case node: ThisExpression => this(node)
      case node: VariableDeclarationExpression => this(node)
      case literal: LiteralExpression => this(literal)
    }
  }

  def apply(expression: ArrayAccessExpression) {
    this(expression.reference)
    this(expression.index)
  }

  def apply(expression: ArrayCreationExpression) {
    this(expression.size)
  }

  def apply(expression: AssignmentExpression) {
    this(expression.left)
    this(expression.right)
  }

  def apply(expression: BooleanLiteral) {}

  def apply(expression: CastExpression) {
    this(expression.expression)
  }

  def apply(expression: CharacterLiteral) {}

  def apply(expression: ClassInstanceCreationExpression) {
    expression.arguments.foreach(this.apply)
  }

  def apply(expression: FieldAccessExpression) {
    this(expression.expression)
  }

  def apply(expression: InfixExpression) {
    this(expression.left)
    this(expression.right)
  }

  def apply(expression: InstanceOfExpression) {
    this(expression.expression)
  }

  def apply(expression: IntegerLiteral) {}

  def apply(expression: MethodInvocationExpression) {
    expression.expression.foreach(this.apply)
    expression.arguments.foreach(this.apply)
  }

  def apply(expression: NullLiteral) {}

  def apply(expression: ParenthesizedExpression) {
    this(expression.expression)
  }

  def apply(expression: PrefixExpression) {
    this(expression.operand)
  }

  def apply(expression: QualifiedNameExpression) {}

  def apply(expression: SimpleNameExpression) {}

  def apply(expression: StringLiteral) {}

  def apply(expression: ThisExpression) {}

  def apply(expression: VariableDeclarationExpression) {}
}

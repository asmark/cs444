package joos.ast

import joos.tokens.TerminalToken

trait Expression extends AstNode

case class ArrayAccessExpression(reference: Expression, index: Expression) extends Expression

case class ArrayCreationExpression(arrType: Type, size: Expression) extends Expression

case class AssignmentExpression(left: Expression, right: Expression) extends Expression

case class CastExpression(val objType: Type, expr: Expression) extends Expression

case class ClassCreationExpression(classType: Type, args: Seq[Expression]) extends Expression

case class FieldAccessExpression(expr: Expression, identifier: SimpleNameExpression) extends Expression

case class InfixExpression(left: Expression, operator: TerminalToken, right: Expression) extends Expression

case class InstanceOfExpression(expr: Expression, refType: Type) extends Expression

case class MethodInvocationExpression(
  expr: Option[Expression],
  methodName: SimpleNameExpression,
  args: Seq[Expression]
) extends Expression

case class ParenthesizedExpression(expr: Expression) extends Expression

case class PrefixExpression(operator: TerminalToken, operand: Expression) extends Expression

case class VariableDeclarationExpression(
  modifiers: Seq[Modifier],
  varType: Type,
  decl: VariableDeclarationFragment
) extends Expression

trait NameExpression extends Expression

case class QualifiedNameExpression(qualifier: NameExpression, name: SimpleNameExpression)

case class SimpleNameExpression(identifier: TerminalToken)

case class IntegerLiteral(token: TerminalToken) extends Expression

case class NullLiteral(token: TerminalToken) extends Expression

case class BooleanLiteral(token: TerminalToken) extends Expression

case class StringLiteral(token: TerminalToken) extends Expression

case class TypeLiteral(staticType: Type) extends Expression

package joos.ast

import joos.tokens.TerminalToken

trait Expression extends AstNode {

}

case class ArrayAccessExpression(reference: Expression, index: Expression)

case class ArrayCreationExpression(arrType: Type, size: Expression)

case class AssignmentExpression(left: Expression, right: Expression)

case class CastExpression(val objType: Type, expr: Expression)

case class ClassCreationExpression(classType: Type, args: Seq[Expression])

case class FieldAccessExpression(expr: Expression, identifier: TerminalToken)

case class InfixExpression(left: Expression, operator: TerminalToken, right: Expression)

case class InstanceOfExpression(expr: Expression, refType: Type)

case class MethodInvocationExpression(expr: Option[Expression], methodName: TerminalToken, args: Seq[Expression])

case class ParenthesizedExpression(expr: Expression)

case class PrefixExpression(operator: TerminalToken, operand: Expression)

case class VariableDeclarationExpression(modifiers: Seq[Modifier], varType: Type, decl: Declaration)




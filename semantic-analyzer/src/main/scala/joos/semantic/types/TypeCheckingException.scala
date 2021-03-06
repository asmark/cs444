package joos.semantic.types

import joos.ast.expressions.{Expression, NameExpression, InfixExpression}
import joos.semantic.SemanticException

class TypeCheckingException(msg: String) extends SemanticException(msg) {
  def this(source: String, errorMessage: String) = this(s"${source}: ${errorMessage}")
}

class ExplicitThisInStaticException(msg: String) extends TypeCheckingException(msg)

class AbstractOrInstanceCreationException(msg: String) extends TypeCheckingException(msg)

class ClassInstanceCreationException(msg: String) extends TypeCheckingException(msg)

class ArrayAccessException(msg: String) extends TypeCheckingException(msg)

class ArrayCreationException(msg: String) extends TypeCheckingException(msg)

class PrefixExpressionException(msg: String) extends TypeCheckingException(msg)

class CastExpressionException(msg: String) extends TypeCheckingException(msg)

class InstanceOfExpressionException(msg: String) extends TypeCheckingException(msg)

class VariableDeclarationExpressionException(msg: String) extends TypeCheckingException(msg)

class AssignmentExpressionException(msg: String) extends TypeCheckingException(msg) {
  def this(left: Expression, right: Expression) = this(s"Cannot assign ${right} to ${left}")
}

class FieldAccessExpressionException(msg: String) extends TypeCheckingException(msg)

class MethodInvocationExpressionException(msg: String) extends TypeCheckingException(msg)

class ParenthesizedExpressionException(msg: String) extends TypeCheckingException(msg)

class MissingConstructorException(msg: String) extends TypeCheckingException(msg)

class InvalidConstructorException(msg: String) extends TypeCheckingException(msg)

class FieldDeclarationTypeException(msg: String) extends TypeCheckingException(msg)

class InfixExpressionException(expression: InfixExpression)
    extends TypeCheckingException(
      "Cannot type check "
          + expression.left.expressionType.standardName
          + ' '
          + expression.operator.name
          + ' '
          + expression.right.expressionType.standardName)

class IllegalProtectedAccessException(name: NameExpression) extends TypeCheckingException(s"Attempted to access non-visible ${name}")

package joos.semantic.types

import joos.semantic.SemanticException
import joos.ast.expressions.InfixExpression

class TypeCheckingException(msg: String) extends SemanticException(msg)

// TODO: Refine the exception
class ImplicitThisInStaticException(msg: String) extends TypeCheckingException(msg)

class AbstractClassCreationException(msg: String) extends TypeCheckingException(msg)

class ClassCreationException(msg: String) extends TypeCheckingException(msg)

class ArrayAccessException(msg: String) extends TypeCheckingException(msg)

class ArrayCreationException(msg: String) extends TypeCheckingException(msg)

class PrefixExpressionException(msg: String) extends TypeCheckingException(msg)

class CastExpressionException(msg: String) extends TypeCheckingException(msg)

class InstanceOfExpressionException(msg: String) extends TypeCheckingException(msg)

class VariableDeclarationExpressionException(msg: String) extends TypeCheckingException(msg)

class AssignmentExpressionException(msg: String) extends TypeCheckingException(msg)

class FieldAccessExpressionException(msg: String) extends TypeCheckingException(msg)

class MethodInvocationExpressionException(msg: String) extends TypeCheckingException(msg)

class ParenthesizedExpressionException(msg: String) extends TypeCheckingException(msg)

class InfixExpressionException(expression: InfixExpression)
    extends TypeCheckingException(
      "Cannot type check "
      + expression.left.declarationType.standardName
      + ' '
      + expression.operator.name
      + ' '
      + expression.right.declarationType.standardName)

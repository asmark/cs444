package joos.semantic.types.checking

import joos.ast.Operator._
import joos.ast.expressions.PrefixExpression
import joos.ast.types.PrimitiveType._
import joos.ast.visitor.AstVisitor
import joos.semantic.types.TypeCheckingException

trait PrefixExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>

  override def apply(prefix: PrefixExpression) {
    prefix.operand.accept(this)
    require(prefix.operand.expressionType != null)

    prefix.expressionType = prefix.operator match {
      case Plus | Minus =>
        if (!prefix.operand.expressionType.isNumeric)
          throw new TypeCheckingException("prefix", s"${prefix.operator} ${prefix.operand.expressionType.standardName}")
        IntegerType
      case Not =>
        if (prefix.operand.expressionType != BooleanType)
          throw new TypeCheckingException("prefix", s"${prefix.operator} ${prefix.operand.expressionType.standardName}")
        BooleanType
    }
  }
}

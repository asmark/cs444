package joos.semantic.types.checking

import joos.ast.visitor.AstVisitor
import joos.ast.expressions.PrefixExpression
import joos.syntax.tokens.{TokenKind, TerminalToken}
import joos.ast.types.PrimitiveType
import joos.semantic.types.PrefixExpressionException
import joos.ast.Operator._

trait PrefixExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(prefixExpression: PrefixExpression) {
    prefixExpression.operand.accept(this)
    require(prefixExpression.operand.declarationType != null)

    prefixExpression.operator match {
      case Plus | Minus => {
        prefixExpression.operand.declarationType match {
          case PrimitiveType.IntegerType => {
//            prefixExpression.declarationType = prefixExpression.operand.declarationType
          }
          case others => throw new PrefixExpressionException(s"invalid operand type ${others.standardName}")
        }
      }
      case Not => {
        prefixExpression.operand.declarationType  match {
          case PrimitiveType.BooleanType => {
//            prefixExpression.declarationType = prefixExpression.operand.declarationType
          }
          case others => throw new PrefixExpressionException(s"invalid operand type ${others.standardName}")
        }
      }
    }
  }
}

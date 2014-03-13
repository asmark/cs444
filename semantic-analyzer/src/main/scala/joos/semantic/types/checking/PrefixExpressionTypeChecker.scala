package joos.semantic.types.checking

import joos.ast.visitor.AstVisitor
import joos.ast.expressions.PrefixExpression
import joos.syntax.tokens.{TokenKind, TerminalToken}
import joos.ast.types.PrimitiveType
import joos.semantic.types.PrefixExpressionException

trait PrefixExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(prefixExpression: PrefixExpression) {
    prefixExpression.operand.accept(this)
    prefixExpression.operator match {
      case TerminalToken(_, TokenKind.Increment | TokenKind.Decrement | TokenKind.Plus | TokenKind.Minus | TokenKind.Tilde) => {
        prefixExpression.operand.declarationType match {
          case PrimitiveType.IntegerType => {
            prefixExpression.declarationType = prefixExpression.operand.declarationType
          }
          case others => throw new PrefixExpressionException(s"invalid operand type ${others.standardName}")
        }
      }
      case TerminalToken(_, TokenKind.Exclamation) => {
        prefixExpression.operand.declarationType  match {
          case PrimitiveType.BooleanType => {
            prefixExpression.declarationType = prefixExpression.operand.declarationType
          }
          case others => throw new PrefixExpressionException(s"invalid operand type ${others.standardName}")
        }
      }
    }
  }
}

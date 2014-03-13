package joos.ast

import joos.semantic.SemanticException
import joos.syntax.tokens.TerminalToken
import joos.syntax.tokens.TokenKind

/**
 * Contains all operators in Joos 1W grammar
 */
object Operator extends Enumeration {
  type Operator = Value

  final val Plus = Value("+")
  final val Minus = Value("-")
  final val Multiply = Value("*")
  final val Divide = Value("/")
  final val Modulo = Value("%")
  final val ConditionalAnd = Value("&&")
  final val ConditionalOr = Value("||")
  final val Greater = Value(">")
  final val Less = Value("<")
  final val Equal = Value("==")
  final val NotEqual = Value("!=")
  final val LessOrEqual = Value("<=")
  final val GreaterOrEqual = Value(">=")
  final val BitwiseAnd = Value("&")
  final val BitwiseInclusiveOr = Value("|")
  final val BitwiseExclusiveOr = Value("^")
  final val BitwiseNot = Value("~")

  def apply(token: TerminalToken): Operator = {
    token.kind match {
      case TokenKind.Plus => Plus
      case TokenKind.Minus => Minus
      case TokenKind.Multiply => Multiply
      case TokenKind.Divide => Divide
      case TokenKind.Modulo => Modulo
      case TokenKind.And => ConditionalAnd
      case TokenKind.Or => ConditionalOr
      case TokenKind.Greater => Greater
      case TokenKind.Less => Less
      case TokenKind.Equal => Equal
      case TokenKind.NotEqual => NotEqual
      case TokenKind.LessEqual => LessOrEqual
      case TokenKind.GreaterEqual => GreaterOrEqual
      case TokenKind.BitAnd => BitwiseAnd
      case TokenKind.BitOr => BitwiseInclusiveOr
      case TokenKind.Carrot => BitwiseExclusiveOr
      case TokenKind.Tilde => BitwiseNot
      case _ => throw new SemanticException(s"${token.lexeme} is not an operator")
    }
  }
}

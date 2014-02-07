package joos.weeder

import joos.parsetree.{LeafNode, ParseTreeNode}
import joos.tokens.{TokenKind, TerminalToken}
import joos.weeder.exceptions.WeederException

case class DecimalIntegerRangeWeeder() extends Weeder {

  private final def ErrorMessage(i: BigInt) = s"${i} cannot exceed the bounds of [${Int.MinValue}, ${Int.MaxValue}]"

  def check(ptn: ParseTreeNode) {
    ptn match {
      case LeafNode(TerminalToken(lexeme, TokenKind.DecimalIntLiteral)) => {
        val value = BigInt(lexeme)
        if ((value < Int.MinValue) || (value > Int.MaxValue)) {
          // TODO: We can't do this. We need to check for "Minus UnaryExpression"
          throw new WeederException(ErrorMessage(value))
        }
      }
      case _ =>
    }
  }
}

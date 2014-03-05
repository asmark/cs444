package joos.weeder

import joos.parser.ParseMetaData
import joos.parsetree.ParseTreeNode

// TODO: Refactor
case class DecimalIntegerRangeWeeder() extends Weeder {

  private final def ErrorMessage(i: BigInt) = s"${i} cannot exceed the bounds of [${Int.MinValue}, ${Int.MaxValue}]"

  // IntegerLiteralNodes that have been checked
  var checkedIntegerLiteralNode = Set[ParseTreeNode]()

  def checkUnaryExpressionNotPlusMinusInRange(unaryExpressionNotPlusMinus: ParseTreeNode, sign: Int): Unit = {
    if (!unaryExpressionNotPlusMinus.token.symbol.equals("UnaryExpressionNotPlusMinus"))
      return

    val primary = unaryExpressionNotPlusMinus.children.head
    if (!primary.token.symbol.equals("Primary"))
      return

    val primaryNoNewArray = primary.children.head
    if (!primaryNoNewArray.token.symbol.equals("PrimaryNoNewArray"))
      return

    val literal = primaryNoNewArray.children.head
    if (!literal.token.symbol.equals("Literal"))
      return

    val integerLiteral = literal.children.head
    if (!integerLiteral.token.symbol.equals("DecimalIntLiteral"))
      return

    if (!checkedIntegerLiteralNode.contains(integerLiteral)) {
      checkedIntegerLiteralNode += integerLiteral
      val intVal = BigInt(integerLiteral.token.lexeme) * sign
      if ((intVal < Int.MinValue) || (intVal > Int.MaxValue)) {
        throw new WeederException(ErrorMessage(intVal))
      }
    }
  }

  def check(ptn: ParseTreeNode, md: ParseMetaData) {
    if (!ptn.token.symbol.equals(UnaryExpression))
      return

    val children = ptn.children
    if (children.head.token.symbol.equals("-")) {
      val unaryExpressionNotPlusMinus = children.last.children.head
      checkUnaryExpressionNotPlusMinusInRange(unaryExpressionNotPlusMinus, -1)
    } else if (children.head.token.symbol.equals("UnaryExpressionNotPlusMinus")) {
      val unaryExpressionNotPlusMinus = children.head
      checkUnaryExpressionNotPlusMinusInRange(unaryExpressionNotPlusMinus, 1)
    }
  }
}

package joos.weeder

import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.tokens.NonTerminalToken
import joos.weeder.exceptions.WeederException
import joos.parser.ParseMetaData

// No field can be final.
case class FieldWeeder() extends Weeder {

  private final val ErrorMessage = "No field can be final"

  override def check(ptn: ParseTreeNode, md: ParseMetaData) {
    ptn match {
      case TreeNode(NonTerminalToken(FieldDeclaration, FieldDeclaration), children) => {
        if (children.head.token.symbol equals Modifiers) {
          val allModifiers = getAllModifiers(children.head)
          if (allModifiers.contains(Final)) throw new WeederException(ErrorMessage)
        }
      }
      case _ =>
    }
  }
}
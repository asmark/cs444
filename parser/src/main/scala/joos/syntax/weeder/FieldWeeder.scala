package joos.syntax.weeder

import joos.syntax.parser.ParseMetaData
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}
import joos.syntax.tokens.NonTerminalToken

// No field can be final.
case class FieldWeeder() extends Weeder {

  private final val ErrorMessage = "No field can be final"

  override def check(ptn: ParseTreeNode, md: ParseMetaData) {
    ptn match {
      case TreeNode(_,NonTerminalToken(FieldDeclaration, FieldDeclaration), children) => {
        if (children.head.token.symbol equals Modifiers) {
          val allModifiers = getAllModifiers(children.head)
          if (allModifiers.contains(Final)) throw new WeederException(ErrorMessage)
        }
      }
      case _ =>
    }
  }
}
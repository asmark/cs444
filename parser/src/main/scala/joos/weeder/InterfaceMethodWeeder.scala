package joos.weeder

import joos.parser.ParseMetaData
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.tokens.NonTerminalToken

// An interface cannot contain fields or constructors.
case class InterfaceMethodWeeder() extends Weeder {

  private final val ErrorMessage = "An interface cannot contain fields or constructors"

  override def check(ptn: ParseTreeNode, md: ParseMetaData) {

    ptn match {
      case TreeNode(_,NonTerminalToken(AbstractMethodDeclaration, AbstractMethodDeclaration), children) => {
        val methodHeader = children.head
        val modifiersNode = methodHeader.children.find(_.token.symbol equals Modifiers)
        if (modifiersNode.isDefined) {
          val modifiers = getAllModifiers(modifiersNode.get)
          if (modifiers.contains(Static) || modifiers.contains(Final) || modifiers.contains(Native)) {
            throw new WeederException(ErrorMessage)
          }
        }
      }
      case _ =>
    }
  }
}
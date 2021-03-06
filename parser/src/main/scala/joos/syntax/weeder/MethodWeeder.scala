package joos.syntax.weeder

import joos.syntax.parser.ParseMetaData
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}
import joos.syntax.tokens.NonTerminalToken

case class MethodWeeder() extends Weeder {

  def check(ptn: ParseTreeNode, md: ParseMetaData) {
    ptn match {
      case TreeNode(_,NonTerminalToken(MethodDeclaration, MethodDeclaration), children) => {
        val header = children.head
        val body = children.last

        val modifiers = header.children.find(_.token.symbol equals Modifiers)
        modifiers match {
          case Some(modifiersNode) =>
            // A method has a body if and only if it is neither abstract nor native.
            val modifiers = getAllModifiers(modifiersNode)
            if (modifiers.contains(Abstract) || modifiers.contains(Native)) {
              if (body.children.head.token.symbol.equals(Block)) {
                throw new WeederException("A method has a body if and only if it is neither abstract nor native.")
              }
            } else {
              if (!body.children.head.token.symbol.equals(Block)) {
                throw new WeederException("A method has a body if and only if it is neither abstract nor native.")
              }
            }

            // An abstract method cannot be static or final.
            if (modifiers.contains(Abstract) && (modifiers.contains(Final) || modifiers.contains(Static))) {
              throw new WeederException("An abstract method cannot be static or final.")
            }

            // A static method cannot be final.
            if (modifiers.contains(Static) && modifiers.contains(Final)) {
              throw new WeederException("A static method cannot be final.")
            }

            // A native method must be static.
            if (modifiers.contains(Native) && !modifiers.contains(Static)) {
              throw new WeederException("A native method must be static.")
            }

            if (!modifiers.contains(Public) && !modifiers.contains(Protected)) {
              throw new WeederException("A method must have either public or protected modifier.")
            }

          case None =>
            // A method has a body if and only if it is neither abstract nor native.
            if (!body.children.head.token.symbol.equals(Block)) {
              throw new WeederException("A method has a body if and only if it is neither abstract nor native.")
            }
        }
      }
      case _ =>
    }

  }
}

package joos.syntax.weeder

import joos.syntax.parser.ParseMetaData
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}
import joos.syntax.tokens.NonTerminalToken

case class ClassModifierWeeder() extends Weeder {

  private final val ErrorMessage = "A class cannot be both abstract and final"

  def check(ptn: ParseTreeNode, md: ParseMetaData) {
    ptn match {
      case TreeNode(_,NonTerminalToken(ClassDeclaration, ClassDeclaration), children) => {
        assert(children.length > 0)
        // A class cannot be both abstract and final.
        children.head match {
          case firstNode: TreeNode => {
            firstNode.token.symbol match {
              case Modifier => {}
              case Modifiers => {
                val allModifers = getAllModifiers(firstNode)
                if (allModifers.contains(Abstract) && allModifers.contains(Final)) {
                  throw new WeederException(ErrorMessage)
                }
              }
              case _ => {}
            }
          }
          case _ => {}
        }
      }
      case _ => {}
    }
  }
}
package joos.weeder

import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.tokens.NonTerminalToken
import joos.weeder.exceptions.WeederException
import joos.parser.ParseMetaData

case class ClassModifierWeeder() extends Weeder {

  private final val ErrorMessage = "A class cannot be both abstract and final"

  def check(ptn: ParseTreeNode, md: ParseMetaData) {
    ptn match {
      case TreeNode(NonTerminalToken(ClassDeclaration, ClassDeclaration), children) => {
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
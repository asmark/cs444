package joos.weeder

import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.tokens.NonTerminalToken
import joos.weeder.exceptions.WeederException

// Every class must contain at least one explicit constructor.
case class ExplicitClassConstructorWeeder() extends Weeder {

  private final val ErrorMessage = "Every class must contain at least one explicit constructor"

  override def check(ptn: ParseTreeNode) {
    ptn match {
      case TreeNode(NonTerminalToken(ClassBody, ClassBody), children) => {
        if (children.length < 3) throw new WeederException(ErrorMessage)
        if (!getAllClassBodyDeclaration(children(1)).contains(ConstructorDeclaration)) throw new WeederException(
          ErrorMessage
        )
      }
      case _ =>
    }
  }
}


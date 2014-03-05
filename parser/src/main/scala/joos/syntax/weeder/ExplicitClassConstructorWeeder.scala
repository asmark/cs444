package joos.syntax.weeder

import joos.syntax.parser.ParseMetaData
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}
import joos.syntax.tokens.NonTerminalToken

// Every class must contain at least one explicit constructor.
case class ExplicitClassConstructorWeeder() extends Weeder {

  private final val ErrorMessage = "Every class must contain at least one explicit constructor"

  override def check(ptn: ParseTreeNode, md: ParseMetaData) {
    ptn match {
      case TreeNode(_,NonTerminalToken(ClassBody, ClassBody), children) => {
        if (children.length < 3) throw new WeederException(ErrorMessage)
        if (!getAllClassBodyDeclaration(children(1)).contains(ConstructorDeclaration)) throw new WeederException(
          ErrorMessage
        )
      }
      case _ =>
    }
  }
}


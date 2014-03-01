package joos.ast

import joos.ast.exceptions.AstConstructionException
import joos.ast.expressions.Expression
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}

case class WhileStatement(condition: Expression, body: Statement)
    extends Statement
    with WhileStatementEnvironmentLinker {
  var enclosingBlock: Block = null
}

object WhileStatement {
  def apply(ptn: ParseTreeNode): WhileStatement = {
    ptn match {
      case TreeNode(ProductionRule(_, Seq("while", "(", "Expression", ")", _)), _, children) =>
        new WhileStatement(Expression(children(2)), Statement(children(4)))
      case _ => throw new AstConstructionException(
        "Invalid tree node to create WhileStatement"
      )
    }
  }
}

trait WhileStatementEnvironmentLinker {
  self: WhileStatement =>

    def link(implicit enclosingBlock: Block): this.type = {
      this.enclosingBlock = enclosingBlock

      this
    }

}

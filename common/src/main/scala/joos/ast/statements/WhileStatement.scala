package joos.ast

import joos.ast.exceptions.AstConstructionException
import joos.ast.expressions.Expression
import joos.ast.statements.WhileStatementLinker
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}

case class WhileStatement(condition: Expression, body: Statement) extends Statement with WhileStatementLinker

object WhileStatement {
  def apply(ptn: ParseTreeNode): WhileStatement = {
    ptn match {
      case TreeNode(ProductionRule(_, Seq("while", "(", "Expression", ")", _)), _, children) =>
        WhileStatement(Expression(children(2)), Statement(children(4)))
      case _ => throw new AstConstructionException(
        "Invalid tree node to create WhileStatement"
      )
    }
  }
}

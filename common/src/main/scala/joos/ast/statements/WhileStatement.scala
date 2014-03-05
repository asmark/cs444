package joos.ast.statements

import joos.ast.AstConstructionException
import joos.ast.expressions.Expression
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}

case class WhileStatement(condition: Expression, body: Statement) extends Statement

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

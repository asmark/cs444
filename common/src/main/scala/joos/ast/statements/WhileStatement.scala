package joos.ast

import joos.ast.expressions.Expression
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException
import joos.semantic.{BlockEnvironment, TypeEnvironment, ModuleEnvironment}

case class WhileStatement(cond: Expression, body: Statement) extends Statement

object WhileStatement {
  def apply(ptn: ParseTreeNode)(
      implicit moduleEnvironment: ModuleEnvironment,
      typeEnvironment: TypeEnvironment,
      blockEnvironment: BlockEnvironment): WhileStatement = {
    ptn match {
      case TreeNode(ProductionRule(_, Seq("while", "(", "Expression", ")", _)), _,  children) =>
        return new WhileStatement(Expression(children(2)), Statement(children(4)))
      case _ => throw new AstConstructionException(
        "Invalid tree node to create WhileStatement"
      )
    }
  }
}

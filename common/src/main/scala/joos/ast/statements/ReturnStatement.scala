package joos.ast

import joos.ast.expressions.Expression
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException
import joos.semantic.{BlockEnvironment, TypeEnvironment, ModuleEnvironment}

case class ReturnStatement(exp: Option[Expression]) extends Statement

object ReturnStatement {
  def apply(ptn: ParseTreeNode)(
      implicit moduleEnvironment: ModuleEnvironment,
      typeEnvironment: TypeEnvironment,
      blockEnvironment: BlockEnvironment): ReturnStatement = {
    ptn match {
      case TreeNode(ProductionRule("ReturnStatement", Seq("return", "Expression", ";")), _, children) =>
        return new ReturnStatement(Some(Expression(children(1))))
      case TreeNode(ProductionRule("ReturnStatement", Seq("return", ";")), _, children) =>
        return new ReturnStatement(None)
      case _ => throw new AstConstructionException(
        "Invalid tree node to create ReturnStatetment"
      )
    }
  }
}
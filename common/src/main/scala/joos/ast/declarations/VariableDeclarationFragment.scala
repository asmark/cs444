package joos.ast.declarations

import joos.ast.expressions.{Expression, SimpleNameExpression}
import joos.ast.{AstConstructionException, AstNode}
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}

case class VariableDeclarationFragment(
  identifier: SimpleNameExpression,
  initializer: Option[Expression]
) extends AstNode

object VariableDeclarationFragment {
  def apply(ptn: ParseTreeNode): VariableDeclarationFragment = {
    ptn match {
      case TreeNode(ProductionRule("VariableDeclarator", Seq("VariableDeclaratorId")), _, children) =>
        VariableDeclarationFragment(SimpleNameExpression(children(0).children(0)), None)
      case TreeNode(
        ProductionRule("VariableDeclarator", Seq("VariableDeclaratorId", "=", "VariableInitializer")),
        _,
        children
      ) =>
        VariableDeclarationFragment(
          SimpleNameExpression(children(0).children(0)),
          Some(Expression(children(2).children(0)))
        )
      case _ => throw new AstConstructionException(
        "Invalid tree node to create VariableDeclarator"
      )
    }
  }
}
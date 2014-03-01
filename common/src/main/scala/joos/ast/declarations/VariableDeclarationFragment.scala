package joos.ast.declarations

import joos.ast.expressions.{Expression, SimpleNameExpression}
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException

case class VariableDeclarationFragment(
  identifier: SimpleNameExpression,
  initializer: Option[Expression]
)

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
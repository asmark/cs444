package joos.ast.declarations

import joos.ast.expressions.{Expression, SimpleNameExpression}
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException
import joos.semantic.{BlockEnvironment, TypeEnvironment, ModuleEnvironment}

case class VariableDeclarationFragment(
  identifier: SimpleNameExpression,
  initializer: Option[Expression]
)

object VariableDeclarationFragment {
  def apply(ptn: ParseTreeNode)(
      implicit moduleEnvironment: ModuleEnvironment,
      typeEnvironment: TypeEnvironment,
      blockEnvironment: BlockEnvironment): VariableDeclarationFragment = {
    ptn match {
      case TreeNode(ProductionRule("VariableDeclarator", Seq("VariableDeclaratorId")), _, children) =>
        return new VariableDeclarationFragment(SimpleNameExpression(children(0).children(0)), None)
      case TreeNode(
        ProductionRule("VariableDeclarator", Seq("VariableDeclaratorId", "=", "VariableInitializer")),
        _,
        children
      ) =>
        return new VariableDeclarationFragment(
          SimpleNameExpression(children(0).children(0)),
          Some(Expression(children(2).children(0)))
        )
      case _ => throw new AstConstructionException(
        "Invalid tree node to create VariableDeclarator"
      )
    }
  }
}
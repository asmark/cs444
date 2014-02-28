package joos.ast.expressions

import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException
import joos.semantic.{BlockEnvironment, TypeEnvironment, ModuleEnvironment}

case class FieldAccessExpression(expression: Expression, identifier: SimpleNameExpression) extends Expression

object FieldAccessExpression {
  def apply(ptn: ParseTreeNode)(
      implicit moduleEnvironment: ModuleEnvironment,
      typeEnvironment: TypeEnvironment,
      blockEnvironment: BlockEnvironment): FieldAccessExpression = {
    ptn match {
      case TreeNode(ProductionRule("FieldAccess", Seq("Primary", ".", "Identifier")), _, children) =>
        return FieldAccessExpression(Expression(children(0)), SimpleNameExpression(children(2)))
      case _ => throw new AstConstructionException("No valid production rule to create FieldAccessExpression")
    }
  }
}

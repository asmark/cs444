package joos.ast.expressions

import joos.ast.exceptions.AstConstructionException
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}

trait NameExpression extends Expression

object NameExpression {
  def apply(ptn: ParseTreeNode): NameExpression = {
    ptn match {
      case TreeNode(ProductionRule("Name", Seq("SimpleName")),_, children) =>
        return SimpleNameExpression(children(0))
      case TreeNode(ProductionRule("Name", Seq("QualifiedName")),_, children) =>
        return QualifiedNameExpression(children(0))
      case _ => throw new AstConstructionException("No valid production rule to make NameExpression")
    }
  }
}

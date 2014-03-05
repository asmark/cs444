package joos.ast.expressions

import joos.ast.AstConstructionException
import joos.ast.compositions.LikeName
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}

trait NameExpression extends Expression with LikeName

object NameExpression {
  def apply(ptn: ParseTreeNode): NameExpression = {
    ptn match {
      case TreeNode(ProductionRule("Name", Seq("SimpleName")), _, children) =>
        SimpleNameExpression(children(0))
      case TreeNode(ProductionRule("Name", Seq("QualifiedName")), _, children) =>
        QualifiedNameExpression(children(0))
      case _ => throw new AstConstructionException("No valid production rule to make NameExpression")
    }
  }

  /**
   * Convenient method for creating {{NameExpression}} from a {{String}}
   */
  def apply(name: String): NameExpression = {
    val names = name.split('.')
    if (names.length == 1) {
      SimpleNameExpression(name)
    } else {
      names.drop(1).foldLeft[NameExpression](SimpleNameExpression(names(0))) {
        (expression, name) =>
          QualifiedNameExpression(expression, SimpleNameExpression(name))
      }
    }
  }
}

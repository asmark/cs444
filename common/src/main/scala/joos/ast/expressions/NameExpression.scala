package joos.ast.expressions

import joos.ast._
import joos.ast.compositions.{DeclarationLike, NameLike}
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.ParseTreeNode
import joos.syntax.parsetree.TreeNode
import joos.ast.NameClassification._

trait NameExpression
    extends Expression
    with NameLike
    with DeclarationReference[DeclarationLike] {

  var nameClassification: NameClassification = Ambiguous
}

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

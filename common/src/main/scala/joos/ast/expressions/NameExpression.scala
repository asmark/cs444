package joos.ast.expressions

import joos.ast.exceptions.AstConstructionException
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.semantic.{BlockEnvironment, TypeEnvironment, ModuleEnvironment}

trait NameExpression extends Expression {
  /**
   * Gets the standard name separated by .
   */
  def standardName: String

  override def toString = standardName

  override def hashCode = standardName.hashCode

  override def equals(that: Any) = that match {
    case that: NameExpression => that.standardName == standardName
    case _ => false
  }
}

object NameExpression {
  def apply(ptn: ParseTreeNode)(
      implicit moduleEnvironment: ModuleEnvironment,
      typeEnvironment: TypeEnvironment,
      blockEnvironment: BlockEnvironment): NameExpression = {
    ptn match {
      case TreeNode(ProductionRule("Name", Seq("SimpleName")), _, children) =>
        return SimpleNameExpression(children(0))
      case TreeNode(ProductionRule("Name", Seq("QualifiedName")), _, children) =>
        return QualifiedNameExpression(children(0))
      case _ => throw new AstConstructionException("No valid production rule to make NameExpression")
    }
  }

  /**
   * Convenient method for creating {{NameExpression}} from a {{String}}
   */
  def apply(name: String)(
      implicit moduleEnvironment: ModuleEnvironment,
      typeEnvironment: TypeEnvironment,
      blockEnvironment: BlockEnvironment): NameExpression = {
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

package joos.ast.expressions

import joos.ast.exceptions.AstConstructionException
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.semantic.{BlockEnvironment, TypeEnvironment, ModuleEnvironment}

case class QualifiedNameExpression(qualifier: NameExpression, name: SimpleNameExpression) extends NameExpression {
  lazy val standardName = qualifier.standardName + '.' + name.standardName
}

object QualifiedNameExpression {
  def apply(ptn: ParseTreeNode)(
      implicit moduleEnvironment: ModuleEnvironment,
      typeEnvironment: TypeEnvironment,
      blockEnvironment: BlockEnvironment): QualifiedNameExpression = {
    ptn match {
      case TreeNode(ProductionRule("QualifiedName", _), _, children) =>
        return QualifiedNameExpression(NameExpression(children(0)), SimpleNameExpression(children(2)))
      case _ => throw new AstConstructionException("No valid production rule to make QualifiedNameExpression")
    }
  }
}


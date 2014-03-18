package joos.ast.expressions

import joos.ast.compositions.DeclarationLike
import joos.ast.{DeclarationReference, AstConstructionException}
import joos.core.MutableLink
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}

case class QualifiedNameExpression(qualifier: NameExpression, name: SimpleNameExpression)
    extends NameExpression
    with DeclarationReference[DeclarationLike] {
  override lazy val standardName = qualifier.standardName + '.' + name.standardName

  def unfold: Seq[SimpleNameExpression] = {
    qualifier match {
      case simpleName: SimpleNameExpression => Seq(simpleName) :+ name
      case qualifiedName: QualifiedNameExpression => qualifiedName.unfold :+ name
    }
  }

  override val declarationLink = new MutableLink[DeclarationLike]
}

object QualifiedNameExpression {
  def apply(ptn: ParseTreeNode): QualifiedNameExpression = {
    ptn match {
      case TreeNode(ProductionRule("QualifiedName", _), _, children) =>
        QualifiedNameExpression(NameExpression(children(0)), SimpleNameExpression(children(2)))
      case _ => throw new AstConstructionException("No valid production rule to make QualifiedNameExpression")
    }
  }
}

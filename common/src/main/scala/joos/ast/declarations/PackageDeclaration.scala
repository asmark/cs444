package joos.ast.declarations

import joos.ast.expressions.NameExpression
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}

case class PackageDeclaration(name: NameExpression) extends Declaration

object PackageDeclaration {
  def apply(ptn: ParseTreeNode): PackageDeclaration = {
    ptn match {
      case TreeNode(ProductionRule("PackageDeclaration", _), _, children) =>
        return PackageDeclaration(NameExpression(children(1)))
    }
  }
}
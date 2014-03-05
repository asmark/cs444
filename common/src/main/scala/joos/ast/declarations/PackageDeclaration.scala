package joos.ast.declarations

import joos.ast.expressions.NameExpression
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}
import joos.ast.AstNode
import joos.ast.compositions.LikeDeclaration

/**
 * @param name fully qualified name of the package
 */
case class PackageDeclaration(name: NameExpression) extends AstNode with LikeDeclaration {
  def declarationName = name
}

object PackageDeclaration {
  /**
   * Make sure we return the same {PackageDeclaration} instance for the same package name
   */
  def apply(ptn: ParseTreeNode): PackageDeclaration = {
    ptn match {
      case TreeNode(ProductionRule("PackageDeclaration", _), _, children) => {
        PackageDeclaration(NameExpression(children(1)))
      }
    }
  }

  def apply(name: String): PackageDeclaration = PackageDeclaration(NameExpression(name))

  def DefaultPackage = PackageDeclaration("")

}

package joos.ast.declarations

import joos.ast.expressions.NameExpression
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.semantic.PackageEnvironment
import scala.collection.mutable

/**
 * @param name fully qualified name of the package
 */
case class PackageDeclaration(name: NameExpression) extends Declaration with PackageEnvironment

object PackageDeclaration {
  /**
   * Make sure we return the same {PackageDeclaration} instance for the same package name
   */
  private[this] val packages = mutable.HashMap.empty[NameExpression, PackageDeclaration]

  def apply(ptn: ParseTreeNode): PackageDeclaration = {
    ptn match {
      case TreeNode(ProductionRule("PackageDeclaration", _), _, children) => {
        val name = NameExpression(children(1))
        packages.getOrElseUpdate(name, new PackageDeclaration(name))
      }
    }
  }

  def apply(name: String): PackageDeclaration = {
    new PackageDeclaration(NameExpression(name))
  }

  final val DefaultPackage = PackageDeclaration("")
}

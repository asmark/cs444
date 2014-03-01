package joos.ast.declarations

import joos.ast.expressions.NameExpression
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}
import scala.collection.mutable
import joos.semantic.PackageEnvironment

/**
 * @param name fully qualified name of the package
 */
class PackageDeclaration private(name: String) extends Declaration with PackageEnvironment

object PackageDeclaration {
  /**
   * Make sure we return the same {PackageDeclaration} instance for the same package name
   */
  private[this] val packages = mutable.HashMap.empty[String, PackageDeclaration]

  def apply(ptn: ParseTreeNode): PackageDeclaration = {
    ptn match {
      case TreeNode(ProductionRule("PackageDeclaration", _), _, children) => {
        val name = NameExpression(children(1)).standardName
        packages.getOrElseUpdate(name, new PackageDeclaration(name))
      }
    }
  }

  def apply(name: String): PackageDeclaration = {
    new PackageDeclaration(NameExpression(name).standardName)
  }
}

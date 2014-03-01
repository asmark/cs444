package joos.ast.declarations

import joos.ast.expressions.NameExpression
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.semantic.{PackageEnvironment, ModuleEnvironment}
import scala.collection.mutable

/**
 * @param name fully qualified name of the package
 */
case class PackageDeclaration private(name: String)(
    implicit moduleEnvironment: ModuleEnvironment,
    environment: PackageEnvironment) extends Declaration

object PackageDeclaration {
  /**
   * Make sure we return the same {PackageDeclaration} instance for the same package name
   */
  private[this] val packages = mutable.HashMap.empty[String, PackageDeclaration]

  def apply(ptn: ParseTreeNode)(implicit moduleEnvironment: ModuleEnvironment): PackageDeclaration = {
    ptn match {
      case TreeNode(ProductionRule("PackageDeclaration", _), _, children) => {
        val name = NameExpression(children(1)).standardName
        packages.getOrElseUpdate(name, new PackageDeclaration(name)(moduleEnvironment, new PackageEnvironment))
      }
    }
  }

  def apply(name: String)(implicit moduleEnvironment: ModuleEnvironment): PackageDeclaration = {
    new PackageDeclaration(NameExpression(name).standardName)(moduleEnvironment, new PackageEnvironment)
  }
}

package joos.ast.declarations

import joos.ast.expressions.NameExpression
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}
import scala.collection.mutable
import joos.semantic.{TypeEnvironment, BlockEnvironment, ModuleEnvironment, PackageEnvironment}

case class PackageDeclaration(name: NameExpression)
    (implicit val moduleEnvironment: ModuleEnvironment) extends Declaration {
  val environment = new PackageEnvironment
}

object PackageDeclaration {
  /**
   * Make sure we return the same instance for the same package name
   */
  private[this] val packages = mutable.HashMap.empty[NameExpression, PackageDeclaration]

  def apply(ptn: ParseTreeNode)(implicit moduleEnvironment: ModuleEnvironment): PackageDeclaration = {
    implicit val typeEnvironment = new TypeEnvironment
    implicit val blockEnvironment = BlockEnvironment(None) // implicit for NameExpression
    ptn match {
      case TreeNode(ProductionRule("PackageDeclaration", _), _, children) => {
        val name = NameExpression(children(1))
        packages.getOrElseUpdate(name, PackageDeclaration(name))
      }
    }
  }

  def apply(name: String)(implicit  moduleEnvironment: ModuleEnvironment): PackageDeclaration = {
    implicit val typeEnvironment = new TypeEnvironment
    implicit val blockEnvironment = BlockEnvironment(None) // implicit for NameExpression
    PackageDeclaration(NameExpression(name))
  }
}

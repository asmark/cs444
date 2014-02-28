package joos.ast

import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException
import joos.ast.expressions.NameExpression
import joos.semantic.{BlockEnvironment, TypeEnvironment, ModuleEnvironment}

case class ArrayType(elementType: Type, dimensions: Int = 1) extends Type

object ArrayType {
  def apply(ptn: ParseTreeNode)(
      implicit moduleEnvironment: ModuleEnvironment,
      typeEnvironment: TypeEnvironment,
      blockEnvironment: BlockEnvironment): ArrayType = {
    ptn match {
      case TreeNode(ProductionRule("ArrayType", Seq("PrimitiveType", "[", "]")), _,  children) =>
        return new ArrayType(PrimitiveType(children(0)))
      case TreeNode(ProductionRule("ArrayType", Seq("Name", "[", "]")), _,  children) =>
        return new ArrayType(SimpleType(children(0)))
      case _ => throw new AstConstructionException(
        "Invalid tree node to create ArrayType"
      )
    }
  }
}
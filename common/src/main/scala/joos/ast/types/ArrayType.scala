package joos.ast

import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException
import joos.ast.expressions.NameExpression

case class ArrayType(elementType: Type, dimensions: Int = 1) extends Type

object ArrayType {
  def apply(ptn: ParseTreeNode): ArrayType = {
    ptn match {
      case TreeNode(ProductionRule("ArrayType", Seq("PrimitiveType", "[", "]")), _,  children) =>
        ArrayType(PrimitiveType(children(0)))
      case TreeNode(ProductionRule("ArrayType", Seq("Name", "[", "]")), _,  children) =>
        ArrayType(SimpleType(children(0)))
      case _ => throw new AstConstructionException(
        "Invalid tree node to create ArrayType"
      )
    }
  }
}
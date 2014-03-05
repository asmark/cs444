package joos.ast.types

import joos.ast.AstConstructionException
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}

case class ArrayType(elementType: Type, dimensions: Int = 1) extends Type

object ArrayType {
  def apply(ptn: ParseTreeNode): ArrayType = {
    ptn match {
      case TreeNode(ProductionRule("ArrayType", Seq("PrimitiveType", "[", "]")), _, children) =>
        ArrayType(PrimitiveType(children(0)))
      case TreeNode(ProductionRule("ArrayType", Seq("Name", "[", "]")), _, children) =>
        ArrayType(SimpleType(children(0)))
      case _ => throw new AstConstructionException(
        "Invalid tree node to create ArrayType"
      )
    }
  }
}

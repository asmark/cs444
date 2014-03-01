package joos.ast

import joos.ast.exceptions.AstConstructionException
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.semantic.CompilationUnitEnvironment

case class ArrayType(elementType: Type, dimensions: Int = 1) extends Type

object ArrayType {
  def apply(ptn: ParseTreeNode)(
    implicit compilationUnitEnvironment: CompilationUnitEnvironment
  ): ArrayType = {
    ptn match {
      case TreeNode(ProductionRule("ArrayType", Seq("PrimitiveType", "[", "]")), _, children) =>
        return new ArrayType(PrimitiveType(children(0)))
      case TreeNode(ProductionRule("ArrayType", Seq("Name", "[", "]")), _, children) =>
        return new ArrayType(SimpleType(children(0)))
      case _ => throw new AstConstructionException(
        "Invalid tree node to create ArrayType"
      )
    }
  }
}

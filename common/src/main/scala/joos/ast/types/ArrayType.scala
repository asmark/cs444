package joos.ast

import joos.parsetree.ParseTreeNode

case class ArrayType(elementType: Type, dimensions: Int = 1) extends Type

object ArrayType {
  def apply(ptn: ParseTreeNode): ArrayType = {
//    ptn match {
//      case TreeNode(ProductionRule("ArrayType", Seq("PrimitiveType", "(", ")")), _,  children) =>
//        return ArrayType(PrimitiveType(children(0)))
//    }
    null
  }
}
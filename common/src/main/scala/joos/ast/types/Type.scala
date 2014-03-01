package joos.ast

import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException
import joos.ast.expressions.NameExpression

trait Type extends AstNode

object Type {
  def handleReferenceType(referenceType: ParseTreeNode): Type = {
    referenceType match {
      case TreeNode(ProductionRule("ReferenceType", Seq("ClassOrInterfaceType")), _, children) =>
        SimpleType(children(0).children(0))
      case TreeNode(ProductionRule("ReferenceType", Seq("ArrayType")), _, children) =>
        ArrayType(children(0))
      case _ => throw new AstConstructionException(
        "Invalid tree node to create ReferenceType"
      )
    }
  }

  def apply(ptn: ParseTreeNode): Type = {
    ptn match {
      case TreeNode(ProductionRule("Type",  Seq("PrimitiveType")), _, children) =>
        PrimitiveType(children(0))
      case TreeNode(ProductionRule("Type",  Seq("ReferenceType")), _, children) =>
        Type(children(0))
      case TreeNode(ProductionRule("ReferenceType", Seq("ClassOrInterfaceType")), _, children) =>
        Type(children(0))
      case TreeNode(ProductionRule("ReferenceType", Seq("ArrayType")), _, children) =>
        ArrayType(children(0))
      case TreeNode(ProductionRule("ClassOrInterfaceType", Seq("Name")), _, children) =>
        SimpleType(children(0))
      case TreeNode(ProductionRule("PrimitiveType", _), _, children) =>
        PrimitiveType(ptn)
      case _ => throw new AstConstructionException("Invalid tree node to create Type")
    }
  }
}
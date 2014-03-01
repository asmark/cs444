package joos.ast

import joos.ast.exceptions.AstConstructionException
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.semantic.CompilationUnitEnvironment

trait Type extends AstNode

object Type {
  def handleReferenceType(referenceType: ParseTreeNode)(
    implicit compilationUnitEnvironment: CompilationUnitEnvironment
  ): Type = {
    referenceType match {
      case TreeNode(ProductionRule("ReferenceType", Seq("ClassOrInterfaceType")), _, children) =>
        return SimpleType(children(0).children(0))
      case TreeNode(ProductionRule("ReferenceType", Seq("ArrayType")), _, children) =>
        return ArrayType(children(0))
      case _ => throw new AstConstructionException(
        "Invalid tree node to create ReferenceType"
      )
    }
  }

  def apply(ptn: ParseTreeNode)(
    implicit compilationUnitEnvironment: CompilationUnitEnvironment
  ): Type = {
    ptn match {
      case TreeNode(ProductionRule("Type", Seq("PrimitiveType")), _, children) =>
        return PrimitiveType(children(0))
      case TreeNode(ProductionRule("Type", Seq("ReferenceType")), _, children) =>
        return handleReferenceType(children(0))
      case _ => throw new AstConstructionException("Invalid tree node to create Type")
    }
  }
}

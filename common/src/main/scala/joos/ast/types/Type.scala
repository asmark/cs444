package joos.ast.types

import joos.ast.expressions.NameExpression
import joos.ast.types.PrimitiveType._
import joos.ast.{AstConstructionException, AstNode}
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}
import joos.ast.declarations.TypeDeclaration

trait Type extends AstNode {

  def standardName: String

  var declaration: Option[TypeDeclaration]

  override def toString = standardName

  def isNumeric: Boolean = this match {
    case IntegerType | ByteType | CharType | ShortType => true
    case _ => false
  }

  def isReferenceType: Boolean = this match {
    case t: ArrayType => true
    case t: SimpleType => true
    case _ => false
  }
}

object Type {
  def apply(name: String): Type = {
    SimpleType(NameExpression(name))
  }

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
      case TreeNode(ProductionRule("Type", Seq("PrimitiveType")), _, children) =>
        PrimitiveType(children(0))
      case TreeNode(ProductionRule("Type", Seq("ReferenceType")), _, children) =>
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

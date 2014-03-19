package joos.ast.types

import joos.ast.AstConstructionException
import joos.ast.Modifier._
import joos.ast.declarations.{VariableDeclarationFragment, FieldDeclaration, TypeDeclaration}
import joos.ast.expressions.SimpleNameExpression
import joos.ast.types.PrimitiveType._
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.ParseTreeNode
import joos.syntax.parsetree.TreeNode

case class ArrayType(elementType: Type, dimensions: Int = 1) extends Type {
  override def equals(that: Any) = {
    that match {
      case ArrayType(thatElementType, _) => thatElementType == elementType
      case _ => false
    }
  }

  override def standardName = elementType.standardName + (0 until dimensions).map(_ => "[]").mkString
}

object ArrayType {
  final val Length = FieldDeclaration(
    Seq(Final, Public),
    IntegerType,
    VariableDeclarationFragment(SimpleNameExpression("length"), None))

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

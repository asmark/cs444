package joos.ast.expressions

import joos.ast._
import joos.ast.types.{Type, PrimitiveType, SimpleType, ArrayType}
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.ParseTreeNode
import joos.syntax.parsetree.TreeNode

case class CastExpression(castType: Type, expression: Expression) extends Expression {
  override def toString = s"(${castType}) ${expression}"
}

object CastExpression {
  def apply(ptn: ParseTreeNode): CastExpression = {
    ptn match {
      case TreeNode(ProductionRule("CastExpression", Seq("(", "PrimitiveType", ")", "UnaryExpression")), _, children) =>
        CastExpression(PrimitiveType(children(1)), Expression(children(3)))
      case TreeNode(ProductionRule("CastExpression", Seq("(", "PrimitiveType", "Dims", ")", "UnaryExpression")), _, children) =>
        CastExpression(ArrayType(PrimitiveType(children(1))), Expression(children(4)))
      case TreeNode(ProductionRule("CastExpression", Seq("(", "Expression", ")", "UnaryExpressionNotPlusMinus")), _, children) =>
        Expression(children(1)) match {
          case name: NameExpression => CastExpression(SimpleType(name), Expression(children(3)))
          case _ => throw new AstConstructionException("CastExpression expanded to Expression that did not expand to a Type")
        }
      case TreeNode(ProductionRule("CastExpression", Seq("(", "Name", "Dims", ")", "UnaryExpressionNotPlusMinus")), _, children) =>
        CastExpression(ArrayType(SimpleType(children(1))), Expression(children(4)))
      case _ => throw new AstConstructionException("Invalid parse tree node to create CastExpression")
    }
  }
}

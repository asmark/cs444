package joos.ast.expressions

import joos.ast.{SimpleType, PrimitiveType, ArrayType, Type}
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.ast.exceptions.AstConstructionException
import joos.semantic.{BlockEnvironment, TypeEnvironment, ModuleEnvironment}

case class CastExpression(castType: Type, expr: Expression) extends Expression

object CastExpression {
  def apply(ptn: ParseTreeNode)(
      implicit moduleEnvironment: ModuleEnvironment,
      typeEnvironment: TypeEnvironment,
      blockEnvironment: BlockEnvironment): CastExpression = {
    ptn match {
      case TreeNode(ProductionRule("CastExpression", Seq("(", "PrimitiveType", ")", "UnaryExpression")), _, children) =>
        return CastExpression(PrimitiveType(children(1)), Expression(children(3)))
      case TreeNode(ProductionRule("CastExpression", Seq("(", "PrimitiveType", "Dims", ")", "UnaryExpression")), _, children) =>
        return CastExpression(ArrayType(PrimitiveType(children(1))), Expression(children(4)))
      case TreeNode(ProductionRule("CastExpression", Seq("(", "Expression", ")", "UnaryExpressionNotPlusMinus")), _, children) =>
        Expression(children(1)) match {
          case name: NameExpression => return CastExpression(SimpleType(name), Expression(children(3)))
          case _ => throw new AstConstructionException("CastExpression expanded to Expression that did not expand to a Type")
        }
      case TreeNode(ProductionRule("CastExpression", Seq("(", "Name", "Dims", ")", "UnaryExpressionNotPlusMinus")), _, children) =>
        return CastExpression(SimpleType(children(1)), Expression(children(4)))
      case _ => throw new AstConstructionException("Invalid parse tree node to create CastExpression")
    }
  }
}

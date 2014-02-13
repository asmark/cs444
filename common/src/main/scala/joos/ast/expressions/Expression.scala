package joos.ast.expressions

import joos.ast.AstNode
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}

trait Expression extends AstNode

object Expression {
  def apply(ptn: ParseTreeNode): Expression = {
    ptn match {
      // Recursive call to Expression
      // TODO: Trim these down by pattern matching ProductionRule(_,Seq(_)) in lowest precedence
      case TreeNode(ProductionRule("Expression", Seq("AssignmentExpression")), _, children) =>
        return Expression(children(0))
      case TreeNode(ProductionRule("AssignmentExpression", Seq("ConditionalExpression")), _, children) =>
        return Expression(children(0))
      case TreeNode(ProductionRule("ConditionalOrExpression", Seq("ConditionalAndExpression")), _, children) =>
        return Expression(children(0))
      case TreeNode(ProductionRule("ConditionalAndExpression", Seq("InclusiveOrExpression")), _, children) =>
        return Expression(children(0))
      case TreeNode(ProductionRule("InclusiveOrExpression", Seq("ExclusiveOrExpression")), _, children) =>
        return Expression(children(0))
      case TreeNode(ProductionRule("ExclusiveOrExpression", Seq("AndExpression")), _, children) =>
        return Expression(children(0))
      case TreeNode(ProductionRule("AndExpression", Seq("EqualityExpression")), _, children) =>
        return Expression(children(0))
      case TreeNode(ProductionRule("EqualityExpression", Seq("RelationalExpression")), _, children) =>
        return Expression(children(0))
      case TreeNode(ProductionRule("RelationalExpression", Seq("AdditiveExpression")), _, children) =>
        return Expression(children(0))
      case TreeNode(ProductionRule("AdditiveExpression", Seq("MultiplicativeExpression")), _, children) =>
        return Expression(children(0))
      case TreeNode(ProductionRule("MultiplicativeExpression", Seq("UnaryExpression")), _, children)=>
        return Expression(children(0))
      case TreeNode(ProductionRule("UnaryExpression", Seq("UnaryExpressionNotPlusMinus")), _, children) =>
        return Expression(children(0))
      case TreeNode(ProductionRule("UnaryExpressionNotPlusMinus", Seq("Primary")), _, children) =>
        return Expression(children(0))
      case TreeNode(ProductionRule("Primary", Seq("PrimaryNoNewArray")), _, children) =>
        return Expression(children(0))

      // Concrete Expressions
      case TreeNode(ProductionRule(_, Seq("Name")), _, children) =>
        return NameExpression(children(0))
      case TreeNode(ProductionRule("UnaryExpressionNotPlusMinus", Seq("CastExpression")), _, children) =>
        return CastExpression(children(0))
      case TreeNode(ProductionRule("LeftHandSide", Seq("FieldAccess")), _, children) =>
        return FieldAccessExpression(children(0))
      case TreeNode(ProductionRule("LeftHandSide", Seq("ArrayAccess")), _, children) =>
        return ArrayAccessExpression(children(0))
      case TreeNode(ProductionRule("AssignmentExpression", Seq("Assignment")), _, children) =>
        return AssignmentExpression(children(0))
      case TreeNode(ProductionRule(
          "ConditionalOrExpression" | "ConditionalAndExpression" | "InclusiveOrExpression" |
          "ExclusiveOrExpression" | "AndExpression" | "EqualityExpression" | "RelationalExpression" |
          "AdditiveExpression" | "MultiplicativeExpression", _), _, children) =>
        return InfixExpression(ptn)
      case TreeNode(ProductionRule(_, Seq(_, "UnaryExpression")), _, children) =>
        return PrefixExpression(ptn)
    }
  }
}

// TODO: Do we need this?
//case class TypeLiteral(staticType: Type) extends Expression

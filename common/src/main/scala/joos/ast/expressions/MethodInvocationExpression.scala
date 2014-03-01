package joos.ast.expressions

import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException

case class MethodInvocationExpression(
   expression: Option[Expression],
   methodName: NameExpression,
   args: Seq[Expression]
 ) extends Expression

object MethodInvocationExpression {
  def apply(ptn: ParseTreeNode): MethodInvocationExpression = {
    ptn match {
      case TreeNode(ProductionRule("MethodInvocation", Seq("Name", "(", ")")), _, children) =>
        MethodInvocationExpression(None, NameExpression(children(0)), Seq.empty)
      case TreeNode(ProductionRule("MethodInvocation", Seq("Name", "(", "ArgumentList", ")")), _, children) =>
        MethodInvocationExpression(None, NameExpression(children(0)), Expression.argList(children(2)))
      case TreeNode(ProductionRule("MethodInvocation", Seq("Primary", ".", "Identifier", "(", ")")), _, children) =>
        MethodInvocationExpression(Some(Expression(children(0))), SimpleNameExpression(children(2)), Seq.empty)
      case TreeNode(ProductionRule("MethodInvocation", Seq("Primary", ".", "Identifier", "(", "ArgumentList", ")")), _, children) =>
        MethodInvocationExpression(Some(Expression(children(0))), SimpleNameExpression(children(2)), Expression.argList(children(4)))
      case _ => throw new AstConstructionException("No valid production rule to make MethodInvocationExpression")
    }
  }
}
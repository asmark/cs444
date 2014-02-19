package joos.ast.declarations

import joos.ast.expressions.{Expression, SimpleNameExpression}
import joos.ast.{Type, Modifier}
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException

case class SingleVariableDeclaration(
   modifiers: Seq[Modifier],
   variableType: Type,
   identifier: SimpleNameExpression,
   initializer: Option[Expression]
 ) extends VariableDeclaration

object SingleVariableDeclaration {
  def createFormalParameterNodes(ptn: ParseTreeNode): Seq[SingleVariableDeclaration] = {
    ptn match {
      case TreeNode(ProductionRule("FormalParameterList",  Seq("FormalParameter")), _, children) => {
        return Seq(SingleVariableDeclaration(children(0)))
      }
      case TreeNode(
        ProductionRule("FormalParameterList",  Seq("FormalParameterList", ",", "FormalParameter")),
        _,
        children
      ) => {
        return createFormalParameterNodes(children(0)) ++ Seq(SingleVariableDeclaration(children(2)))
      }
      case _ => throw new AstConstructionException("Invalid tree node to create FormalParameterList")
    }
  }

  // TODO: consolidate this class with FormalParameter
  def apply(ptn: ParseTreeNode): SingleVariableDeclaration = {
    ptn match {
      case TreeNode(ProductionRule("FormalParameter", Seq("Type", "VariableDeclaratorId")), _, children) =>
        return SingleVariableDeclaration(null, Type(children(0)), SimpleNameExpression(children(1).children(0)), None)
      case _ => throw new AstConstructionException("Invalid tree node to create FormalParameter")
    }
  }
}
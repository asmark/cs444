package joos.ast.declarations

import joos.ast.exceptions.AstConstructionException
import joos.ast.expressions.{Expression, SimpleNameExpression}
import joos.ast.{TypedDeclaration, Type, Modifier}
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}

case class SingleVariableDeclaration(
    modifiers: Seq[Modifier],
    variableType: Type,
    identifier: SimpleNameExpression,
    initializer: Option[Expression]) extends VariableDeclaration with TypedDeclaration {

  override def declarationName = identifier

  override def declarationType = variableType
}

object SingleVariableDeclaration {
  def createFormalParameterNodes(ptn: ParseTreeNode): IndexedSeq[SingleVariableDeclaration] = {
    ptn match {
      case TreeNode(ProductionRule("FormalParameterList", Seq("FormalParameter")), _, children) => {
        IndexedSeq(SingleVariableDeclaration(children(0)))
      }
      case TreeNode(
      ProductionRule("FormalParameterList", Seq("FormalParameterList", ",", "FormalParameter")), _, children) => {
        createFormalParameterNodes(children(0)) ++ IndexedSeq(SingleVariableDeclaration(children(2)))
      }
      case _ => throw new AstConstructionException("Invalid tree node to create FormalParameterList")
    }
  }

  def apply(ptn: ParseTreeNode): SingleVariableDeclaration = {
    ptn match {
      case TreeNode(ProductionRule("FormalParameter", Seq("Type", "VariableDeclaratorId")), _, children) =>
        SingleVariableDeclaration(Seq(), Type(children(0)), SimpleNameExpression(children(1).children(0)), None)
      case _ => throw new AstConstructionException("Invalid tree node to create FormalParameter")
    }
  }
}

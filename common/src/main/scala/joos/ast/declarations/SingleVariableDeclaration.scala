package joos.ast.declarations

import joos.ast.AstConstructionException
import joos.ast.Modifier
import joos.ast.compositions.TypedDeclarationLike
import joos.ast.expressions.{Expression, SimpleNameExpression}
import joos.ast.types.Type
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}

case class SingleVariableDeclaration(
    modifiers: Seq[Modifier],
    variableType: Type,
    identifier: SimpleNameExpression,
    initializer: Option[Expression]) extends VariableDeclaration with TypedDeclarationLike {

  override def declarationName = identifier

  override def declarationType = variableType

  override def toString = {
    val initializerText = initializer match {
      case None => ""
      case Some(initializer) => "= " + initializer.toString
    }
    s"${modifiers.mkString(" ")} ${variableType} ${identifier} ${initializerText}"
  }
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

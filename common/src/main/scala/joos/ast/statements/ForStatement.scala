package joos.ast

import joos.ast.expressions.{VariableDeclarationExpression, Expression}
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException
import joos.semantic.{BlockEnvironment, TypeEnvironment, ModuleEnvironment}

case class ForStatement(
  forInit: Option[Expression],
  cond: Option[Expression],
  forUpdate: Option[Expression],
  body: Statement) extends Statement

object ForStatement {
  def apply(ptn: ParseTreeNode)(
      implicit moduleEnvironment: ModuleEnvironment,
      typeEnvironment: TypeEnvironment,
      blockEnvironment: BlockEnvironment): ForStatement = {
    ptn match {
      case TreeNode(
          ProductionRule("ForStatement" | "ForStatementNoShortIf", derivation),
          _,
          children
        ) => {
        val init = derivation.indexOf("ForInit")
        val cond = derivation.indexOf("Expression")
        val update = derivation.indexOf("ForUpdate")
        var body = derivation.indexOf("Statement")
        if (body < 0)
          body = derivation.indexOf("StatementNoShortIf")

        return new ForStatement(
          if (init >= 0) Some(
            children(init) match {
              case TreeNode(ProductionRule("ForInit", Seq("StatementExpression")), _, children) =>
                ExpressionStatement.constructStatementExpression(children(0))
              case TreeNode(ProductionRule("ForInit", Seq("LocalVariableDeclaration")), _, children) =>
                VariableDeclarationExpression(children(0))
            }
          ) else None,
          if (cond >= 0) Some(Expression(children(cond))) else None,
          if (update >= 0) Some(ExpressionStatement.constructStatementExpression(children(update).children(0))) else None,
          Statement(children(body))
        )
      }
      case _ => throw new AstConstructionException("Invalid tree node to create ForStatement")
    }
  }
}

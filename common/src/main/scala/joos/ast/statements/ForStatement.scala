package joos.ast.statements

import joos.ast.AstConstructionException
import joos.ast.expressions.{VariableDeclarationExpression, Expression}
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}
import joos.semantic.BlockEnvironment

case class ForStatement(
  initialization: Option[Expression],
  condition: Option[Expression],
  update: Option[Expression],
  body: Statement) extends Statement {
  override var environment: BlockEnvironment = null

  override def toString = {
    val header = (List(initialization, condition, update) map {
      header => header match {
        case None => ""
        case Some(expression) => expression.toString
      }
    }).mkString("; ")

    s"for(${header}) {\n${body}\n}\n"
  }
}

object ForStatement {
  def apply(ptn: ParseTreeNode): ForStatement = {
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

        new ForStatement(
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

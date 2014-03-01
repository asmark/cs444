package joos.ast.expressions

import joos.ast.exceptions.AstConstructionException
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}

case class AssignmentExpression(left: Expression, right: Expression) extends Expression

object AssignmentExpression {
   def apply(ptn: ParseTreeNode): AssignmentExpression = {
     ptn match {
       case TreeNode(ProductionRule("Assignment", _), _, children) =>
         AssignmentExpression(Expression(children(0)), Expression(children(2)))
       case _ => throw new AstConstructionException("No valid production rule to make AssignmentExpression")
     }
   }
 }

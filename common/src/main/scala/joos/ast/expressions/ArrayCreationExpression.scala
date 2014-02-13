package joos.ast.expressions

import joos.ast.exceptions.AstConstructionException
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.ast.Type

case class ArrayCreationExpression(arrType: Type, size: Expression) extends Expression

object ArrayCreationExpression {
   def apply(ptn: ParseTreeNode): ArrayCreationExpression = {
     ptn match {
       case TreeNode(ProductionRule("ArrayCreationExpression", _), _, children) =>
         return ArrayCreationExpression(Type(children(1)), Expression(children(3)))
       case _ => throw new AstConstructionException("No valid production rule to make ArrayCreationExpression")
     }
   }
 }

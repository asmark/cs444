package joos.ast.expressions

import joos.ast.AstConstructionException
import joos.ast.types.Type
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}

case class ArrayCreationExpression(arrayType: Type, size: Expression) extends Expression

object ArrayCreationExpression {
   def apply(ptn: ParseTreeNode): ArrayCreationExpression = {
     ptn match {
       case TreeNode(ProductionRule("ArrayCreationExpression", derivation), _, children) if derivation.length == 5 =>
         ArrayCreationExpression(Type(children(1)), Expression(children(3)))
       case _ => throw new AstConstructionException("No valid production rule to make ArrayCreationExpression")
     }
   }
 }

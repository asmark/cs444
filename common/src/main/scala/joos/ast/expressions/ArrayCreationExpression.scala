package joos.ast.expressions

import joos.ast.exceptions.AstConstructionException
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.ast.{PrimitiveType, SimpleType, Type}

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

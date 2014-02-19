package joos.ast.expressions

import joos.ast.exceptions.AstConstructionException
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.ast.{PrimitiveType, SimpleType, Type}

case class ArrayCreationExpression(arrayType: Type, size: Expression) extends Expression

object ArrayCreationExpression {
   def apply(ptn: ParseTreeNode): ArrayCreationExpression = {
     ptn match {
       case TreeNode(ProductionRule("ArrayCreationExpression", derivation), _, children) =>
         return ArrayCreationExpression(
           derivation(1) match {
             case "PrimitiveType" =>
               PrimitiveType(children(1))
             case "ClassOrInterfaceType" =>
              SimpleType(children(1).children(0))
           },
           Expression(children(3))
         )
       case _ => throw new AstConstructionException("No valid production rule to make ArrayCreationExpression")
     }
   }
 }

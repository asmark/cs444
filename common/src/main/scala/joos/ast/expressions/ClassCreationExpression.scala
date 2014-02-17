package joos.ast.expressions

import joos.ast.Type
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException

case class ClassCreationExpression(classType: Type, args: Seq[Expression]) extends Expression

object ClassCreationExpression {
  def apply(ptn: ParseTreeNode): ClassCreationExpression = {
    ptn match {
      case TreeNode(ProductionRule("ClassInstanceCreationExpression", Seq("new", "ClassType", "(", ")")), _, children) =>
        return ClassCreationExpression(Type(children(1)), Seq.empty)
      case TreeNode(ProductionRule("ClassInstanceCreationExpression", Seq("new", "ClassType", "(", "ArgumentList", ")")), _, children) =>
        return ClassCreationExpression(Type(children(1)), Expression.argList(children(3)))
      case _ => throw new AstConstructionException("No valid production rule to create ClassCreationExpression")
    }
  }
}

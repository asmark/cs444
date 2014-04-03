package joos.ast.expressions

import joos.ast.AstConstructionException
import joos.ast.types.{Type, SimpleType}
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}
import joos.ast.declarations.MethodDeclaration

case class ClassInstanceCreationExpression(classType: Type, arguments: IndexedSeq[Expression]) extends Expression {
  /**
   * Constructor it's linked to
   */
  var constructor: MethodDeclaration = _
  override def toString = s"new ${classType}(${arguments.mkString(", ")})"
}

object ClassInstanceCreationExpression {
  def apply(ptn: ParseTreeNode): ClassInstanceCreationExpression = {
    ptn match {
      case TreeNode(ProductionRule("ClassInstanceCreationExpression", Seq("new", "ClassType", "(", ")")), _, children) =>
        ClassInstanceCreationExpression(SimpleType(children(1).children(0).children(0)), IndexedSeq())
      case TreeNode(ProductionRule("ClassInstanceCreationExpression", Seq("new", "ClassType", "(", "ArgumentList", ")")), _, children) =>
        ClassInstanceCreationExpression(SimpleType(children(1).children(0).children(0)), Expression.argList(children(3)))
      case _ => throw new AstConstructionException("No valid production rule to create ClassCreationExpression")
    }
  }
}

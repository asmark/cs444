package joos.ast.expressions

import joos.ast.Type
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.language.ProductionRule
import joos.semantic.{BlockEnvironment, TypeEnvironment, ModuleEnvironment}

// TODO: Probably delete this. it is captured by InfixExpression
case class InstanceOfExpression(expression: Expression, classType: Type) extends Expression

object InstanceOfExpression {
  def apply(ptn: ParseTreeNode)(
      implicit moduleEnvironment: ModuleEnvironment,
      typeEnvironment: TypeEnvironment,
      blockEnvironment: BlockEnvironment): InstanceOfExpression = {
    ptn match {
      case TreeNode(
        ProductionRule("RelationalExpression", Seq("RelationalExpression", "instanceof", "ReferenceType")),
        _,
        children
      ) => {
        return InstanceOfExpression(Expression(children(0)), Type.handleReferenceType(children(2)))
      }
    }
  }
}

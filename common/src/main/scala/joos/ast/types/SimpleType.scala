package joos.ast.types

import joos.ast.expressions.NameExpression
import joos.syntax.parsetree.ParseTreeNode
import joos.ast.declarations.TypeDeclaration

case class SimpleType(name: NameExpression) extends Type{
  override var declaration: Option[TypeDeclaration] = _
}

object SimpleType {
  def apply(ptn: ParseTreeNode): SimpleType = new SimpleType(NameExpression(ptn))
}

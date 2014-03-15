package joos.ast.types

import joos.ast.expressions.NameExpression
import joos.syntax.parsetree.ParseTreeNode
import joos.ast.declarations.TypeDeclaration

case class SimpleType(name: NameExpression) extends Type{
  override var declaration: Option[TypeDeclaration] = _

  override def equals(that: Any) = {
    that match {
      case that: SimpleType => that.declaration == declaration
      case _ => false
    }
  }

  override def standardName = name.standardName
}

object SimpleType {
  def apply(ptn: ParseTreeNode): SimpleType = new SimpleType(NameExpression(ptn))
}

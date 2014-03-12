package joos.ast.compositions

import joos.ast.visitor.AstVisitor

trait TypeCheckable extends LikeTyped {
  def checkType(visitor: AstVisitor)
}

package joos.ast.compositions

import joos.ast.visitor.AstVisitor

trait TypeCheckableLike extends LikeTyped {
  def checkType(visitor: AstVisitor)
}

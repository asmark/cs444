package joos.ast

trait AstNode {
  val parent : Option[AstNode]
}

package joos.ast.declarations

import joos.ast.expressions.{Expression, SimpleNameExpression}
import joos.ast.AstNode
import joos.ast.compositions.LikeDeclaration

trait VariableDeclaration extends AstNode with LikeDeclaration {
   def identifier: SimpleNameExpression
   def initializer: Option[Expression]
 }

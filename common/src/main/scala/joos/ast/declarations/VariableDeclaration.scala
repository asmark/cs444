package joos.ast.declarations

import joos.ast.expressions.{Expression, SimpleNameExpression}
import joos.ast.AstNode
import joos.ast.compositions.DeclarationLike

trait VariableDeclaration extends AstNode with DeclarationLike {
   def identifier: SimpleNameExpression
   def initializer: Option[Expression]
 }

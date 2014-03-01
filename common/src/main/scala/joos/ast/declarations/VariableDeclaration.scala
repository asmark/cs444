package joos.ast.declarations

import joos.ast.expressions.{Expression, SimpleNameExpression}
import joos.parsetree.ParseTreeNode

trait VariableDeclaration extends Declaration {
   def identifier: SimpleNameExpression
   def initializer: Option[Expression]
 }

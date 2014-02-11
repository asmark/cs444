package joos.ast

trait Statement extends AstNode

case class WhileStatement(cond: Expression, body: Statement) extends Statement

case class ForStatement(forInit: Expression, cond: Expression, forUpdate: Expression, body: Statement) extends Statement

case class IfStatement(test: Expression, tStatement: Statement, fStatement: Statement) extends Statement

case class ExpressionStatement(expr: Expression) extends Statement

case class ReturnStatement(exp: Expression) extends Statement

case class TypeDeclarationStatement(decl: BodyDeclaration) extends Statement

case class Block(inner: Statement) extends Statement

case class EmptyStatement() extends Statement




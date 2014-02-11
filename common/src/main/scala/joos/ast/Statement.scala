package joos.ast

trait Statement extends AstNode {

}

case class WhileStatement(cond: Expression, body: Statement)

case class ForStatement(forInit: Expression, cond: Expression, forUpdate: Expression, body: Statement)

case class IfStatement(test: Expression, tStatement: Statement, fStatement: Statement)

case class ExpressionStatement(expr: Expression)

case class ReturnStatement(exp: Expression)

case class TypeDeclarationStatement(decl: Declaration)

case class EmptyStatement()




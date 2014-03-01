package joos.semantic
import joos.ast.expressions.NameExpression

class DuplicatedDeclarationException(name: NameExpression)
  extends SemanticException(s"Duplicated declaration: ${name}")

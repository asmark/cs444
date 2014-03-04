package joos.semantic.names.environments

import joos.semantic.SemanticException
import joos.ast.expressions.NameExpression

class EnvironmentBuildingException(msg: String) extends SemanticException(msg)

class DuplicatedDeclarationException(name: NameExpression) extends SemanticException(s"Duplicated declaration: ${name}")

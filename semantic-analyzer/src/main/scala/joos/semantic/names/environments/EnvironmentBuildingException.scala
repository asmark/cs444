package joos.semantic.names.environments

import joos.semantic.SemanticException
import joos.ast.expressions.NameExpression
import joos.ast.declarations.FieldDeclaration

class EnvironmentBuildingException(msg: String) extends SemanticException(msg)

class DuplicatedDeclarationException(name: NameExpression) extends SemanticException(s"Duplicated declaration: ${name}")

class DuplicatedFieldException(fieldName: NameExpression) extends SemanticException(s"Duplicated field: ${fieldName}")

class DuplicatedVariableException(variableName: NameExpression) extends SemanticException(s"Duplicated variable: ${variableName}")

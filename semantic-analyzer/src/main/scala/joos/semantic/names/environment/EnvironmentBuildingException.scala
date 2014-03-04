package joos.semantic.names.environment

import joos.semantic.SemanticException
import joos.ast.expressions.NameExpression
import joos.ast.declarations.FieldDeclaration
import joos.semantic.names.NameResolutionException

class EnvironmentBuildingException(msg: String) extends NameResolutionException(msg)

class DuplicatedDeclarationException(name: NameExpression) extends SemanticException(s"Duplicated declaration: ${name}")

class DuplicatedFieldException(fieldName: NameExpression) extends SemanticException(s"Duplicated field: ${fieldName}")

class DuplicatedVariableException(variableName: NameExpression) extends SemanticException(s"Duplicated variable: ${variableName}")

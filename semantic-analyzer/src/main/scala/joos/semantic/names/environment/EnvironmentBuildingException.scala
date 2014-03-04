package joos.semantic.names.environment

import joos.semantic.SemanticException
import joos.ast.expressions.NameExpression
import joos.ast.declarations.FieldDeclaration
import joos.semantic.names.NameResolutionException

class EnvironmentBuildingException(msg: String) extends NameResolutionException(msg)

class DuplicatedDeclarationException(name: NameExpression) extends EnvironmentBuildingException(s"Duplicated declaration: ${name}")

class DuplicatedFieldException(fieldName: NameExpression) extends EnvironmentBuildingException(s"Duplicated field: ${fieldName}")

class DuplicatedVariableException(variableName: NameExpression) extends EnvironmentBuildingException(s"Duplicated variable: ${variableName}")

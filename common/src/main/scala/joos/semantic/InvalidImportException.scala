package joos.semantic

import joos.ast.expressions.NameExpression

class InvalidImportException(name: NameExpression)
    extends SemanticException(s"Invalid import: ${name.standardName}")

class DuplicateImportException(name: NameExpression) extends SemanticException(s"Duplicate import: ${name.standardName}")
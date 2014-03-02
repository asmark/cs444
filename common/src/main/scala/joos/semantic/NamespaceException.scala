package joos.semantic

import joos.ast.expressions.NameExpression

class NamespaceException(msg: String) extends SemanticException(msg)

class NamespaceCollisionException(name: NameExpression) extends SemanticException(s"Collision of package/type ${name.standardName}")

class MissingTypeException(name: NameExpression) extends SemanticException(s"${name.standardName} missing from the namespace")

class InvalidImportException(name: NameExpression)
    extends SemanticException(s"Invalid import: ${name.standardName}")

class DuplicateImportException(name: NameExpression) extends SemanticException(s"Duplicate import: ${name.standardName}")
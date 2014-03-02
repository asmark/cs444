package joos.semantic

import joos.ast.expressions.NameExpression

class NamespaceException(msg: String) extends SemanticException(msg)

class NamespaceCollisionException(name: NameExpression) extends SemanticException(s"Collision of package/type ${name.standardName}")

class MissingTypeException(name: NameExpression) extends SemanticException(s"${name.standardName} missing from the namespace")
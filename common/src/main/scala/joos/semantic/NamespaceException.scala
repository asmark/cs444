package joos.semantic

import joos.ast.expressions.NameExpression

class NamespaceException(msg: String) extends SemanticException(msg)

class NamespaceCollisionException(name: NameExpression) extends NamespaceException(s"Collision of package/type ${name.standardName}")

class MissingTypeException(name: NameExpression) extends NamespaceException(s"${name.standardName} missing from the namespace")

class NamespacePrefixException(msg: String) extends NamespaceException(msg)
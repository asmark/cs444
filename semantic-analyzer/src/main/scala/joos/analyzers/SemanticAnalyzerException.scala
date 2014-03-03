package joos.analyzers

import joos.semantic.SemanticException
import joos.ast.expressions.NameExpression

class SemanticAnalyzerException(msg: String) extends SemanticException(msg)

class SimpleHierarchyException(msg: String) extends SemanticAnalyzerException(msg)

class InvalidTypeReferenceException(name: NameExpression) extends SemanticAnalyzerException(s"Could not resolve ${name.standardName}")

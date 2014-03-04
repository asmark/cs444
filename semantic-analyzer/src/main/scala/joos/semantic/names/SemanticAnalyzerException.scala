package joos.semantic.names

import joos.ast.declarations.MethodDeclaration
import joos.ast.expressions.NameExpression
import joos.semantic.SemanticException

class SemanticAnalyzerException(msg: String) extends SemanticException(msg)

class SimpleHierarchyException(msg: String) extends SemanticAnalyzerException(msg)

class InvalidTypeReferenceException(name: NameExpression) extends SemanticAnalyzerException(s"Could not resolve ${name.standardName}")

class CyclicHierarchyException(name: NameExpression) extends SemanticAnalyzerException(s"Cycle detected at ${name.standardName}")

class OverrideStaticMethodException(childMethod: MethodDeclaration, parentMethod: MethodDeclaration)
    extends SemanticAnalyzerException(s"non-static ${childMethod.typedSignature} attempts to override static ${parentMethod.typedSignature}")

class OverrideProtectedMethodException(childMethod: MethodDeclaration, parentMethod: MethodDeclaration)
    extends SemanticAnalyzerException(s"${childMethod.typedSignature} attempts to expand visibility of ${parentMethod.typedSignature}")

class OverrideFinalMethodException(childMethod: MethodDeclaration, parentMethod: MethodDeclaration)
    extends SemanticAnalyzerException(s"${childMethod.typedSignature} attempts to override final ${parentMethod.typedSignature}")

class OverrideReturnTypeException(childMethod: MethodDeclaration, parentMethod: MethodDeclaration)
    extends SemanticAnalyzerException(
      s"${childMethod.typedSignature} with type ${childMethod.returnType.get}} attempts to override" +
          s"${parentMethod.typedSignature} with type ${parentMethod.returnType.get}}"
    )
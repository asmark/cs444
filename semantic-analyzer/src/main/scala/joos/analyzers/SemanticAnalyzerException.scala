package joos.analyzers

import joos.semantic.SemanticException
import joos.ast.expressions.NameExpression
import joos.ast.declarations.MethodDeclaration

class SemanticAnalyzerException(msg: String) extends SemanticException(msg)

class SimpleHierarchyException(msg: String) extends SemanticAnalyzerException(msg)

class InvalidTypeReferenceException(name: NameExpression) extends SemanticAnalyzerException(s"Could not resolve ${name.standardName}")

class CyclicHierarchyException(name: NameExpression) extends SemanticAnalyzerException(s"Cycle detected at ${name.standardName}")

class OverrideStaticMethodException(childMethod: MethodDeclaration, parentMethod: MethodDeclaration)
    extends SemanticAnalyzerException(s"${childMethod.typedSignature} attempts to override ${parentMethod.typedSignature}")

class OverridePublicMethodException(childMethod: MethodDeclaration, parentMethod: MethodDeclaration)
    extends SemanticAnalyzerException(s"${childMethod.typedSignature} attempts to override ${parentMethod.typedSignature}")

class OverrideFinalMethodException(childMethod: MethodDeclaration, parentMethod: MethodDeclaration)
    extends SemanticAnalyzerException(s"${childMethod.typedSignature} attempts to override ${parentMethod.typedSignature}")

class OverrideReturnTypeException(childMethod: MethodDeclaration, parentMethod: MethodDeclaration)
    extends SemanticAnalyzerException(
      s"${childMethod.typedSignature} with type ${childMethod.returnType.get}} attempts to override" +
          s"${parentMethod.typedSignature} with type ${parentMethod.returnType.get}}"
    )
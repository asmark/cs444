package joos.semantic.names.heirarchy

import joos.semantic.SemanticException
import joos.ast.expressions.NameExpression
import joos.ast.declarations.MethodDeclaration
import joos.semantic.names.NameResolutionException

class HierarchyException(msg: String) extends NameResolutionException(msg)

class CyclicHierarchyException(name: NameExpression) extends HierarchyException(s"Cycle detected at ${name.standardName}")

class OverrideStaticMethodException(childMethod: MethodDeclaration, parentMethod: MethodDeclaration)
    extends HierarchyException(s"non-static ${childMethod.typedSignature} attempts to override static ${parentMethod.typedSignature}")

class OverrideProtectedMethodException(childMethod: MethodDeclaration, parentMethod: MethodDeclaration)
    extends HierarchyException(s"${childMethod.typedSignature} attempts to expand visibility of ${parentMethod.typedSignature}")

class OverrideFinalMethodException(childMethod: MethodDeclaration, parentMethod: MethodDeclaration)
    extends HierarchyException(s"${childMethod.typedSignature} attempts to override final ${parentMethod.typedSignature}")

class OverrideReturnTypeException(childMethod: MethodDeclaration, parentMethod: MethodDeclaration)
    extends HierarchyException(
      s"${childMethod.typedSignature} with type ${childMethod.returnType.get}} attempts to override" +
          s"${parentMethod.typedSignature} with type ${parentMethod.returnType.get}}"
    )
package joos.semantic.names.heirarchy

import joos.ast.declarations.{TypeDeclaration, MethodDeclaration}
import joos.ast.expressions.NameExpression
import joos.semantic.names.NameResolutionException

class HierarchyException(msg: String) extends NameResolutionException(msg)

// Simple checks

class InvalidExtendedClassException(extendedType: TypeDeclaration)(implicit typeDeclaration: TypeDeclaration)
    extends HierarchyException(s"${typeDeclaration} extends ${extendedType} which is final")

class InvalidExtendedTypeException(extendedType: TypeDeclaration)(implicit typeDeclaration: TypeDeclaration)
    extends HierarchyException(s"${typeDeclaration} extends ${extendedType} which is not a class")


class InvalidImplementedTypeException(implementedType: TypeDeclaration)(implicit typeDeclaration: TypeDeclaration)
    extends HierarchyException(s"${typeDeclaration} implements ${implementedType} which is not an interface")

class DuplicateImplementedInterfaceException(implementedType: TypeDeclaration)(implicit typeDeclaration: TypeDeclaration)
    extends HierarchyException(s"${typeDeclaration} implements ${implementedType} twice")

// Advanced checks

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

class ConcreteClassAbstractMethodException(methodDeclaration: MethodDeclaration, typeDeclaration: TypeDeclaration)
  extends HierarchyException(s"${methodDeclaration.typedSignature} abstract in ${typeDeclaration}")

class SameMethodSignatureException(signature: String, typeDeclaration: TypeDeclaration)
  extends HierarchyException(s"${signature} duplicated in in ${typeDeclaration}")

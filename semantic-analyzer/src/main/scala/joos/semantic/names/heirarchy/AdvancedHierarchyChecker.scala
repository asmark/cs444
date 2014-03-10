package joos.semantic.names.heirarchy

import joos.ast.declarations.{PackageDeclaration, MethodDeclaration, TypeDeclaration, ModuleDeclaration}
import joos.ast.visitor.AstVisitor
import joos.ast.{Modifier, CompilationUnit}
import joos.semantic._
import scala.collection.mutable


/**
 * AdvancedHierarchyChecker is responsible for the following name resolution checks:
 *
 * The hierarchy must be acyclic.
 * A class or interface must not declare two methods with the same signature (name and parameter types).
 * A class or interface must not contain (declare or inherit) two methods with the same signature but different return types
 * A class that contains (declares or inherits) any abstract methods must be abstract.
 * A non-static method must not replace a static method
 * A method must not replace a method with a different return type.
 * A protected method must not replace a public method.
 * A method must not replace a final method.
 */
class AdvancedHierarchyChecker(implicit module: ModuleDeclaration, unit: CompilationUnit) extends AstVisitor with TypeHierarchyChecker {


  private def checkCyclicHierarchy(typeDeclaration: TypeDeclaration) {

    def withPackage(typeDeclaration: TypeDeclaration) = {
      require(typeDeclaration.packageDeclaration != null)
      (typeDeclaration.packageDeclaration, typeDeclaration)
    }

    val children = mutable.LinkedHashSet.empty[(PackageDeclaration, TypeDeclaration)]

    def visit(typeDeclaration: TypeDeclaration) = {
      if (!(children add withPackage(typeDeclaration))) {
        throw new CyclicHierarchyException(typeDeclaration.name)
      }
    }

    def leave(typeDeclaration: TypeDeclaration) = assert(children remove withPackage(typeDeclaration))

    // Topological sort of dependency graph
    def findCycle(currentType: TypeDeclaration) {
      visit(currentType)
      getSuperType(currentType) ++ getSuperInterfaces(currentType) foreach findCycle
      leave(currentType)
    }

    findCycle(typeDeclaration)
  }

  private def checkReturnType(typeDeclaration: TypeDeclaration) {
    var set: mutable.HashMap[String, MethodDeclaration] = mutable.HashMap()

    val methods = typeDeclaration.inheritedMethods.values.flatten ++ typeDeclaration.methods

    methods.foreach(
      method => {
        if (!set.contains(method.localSignature))
          set += {method.localSignature -> method}
        else {
          set.get(method.localSignature) match {
            case Some(existingMethod) =>
              if (!areEqual(existingMethod.returnType, method.returnType) && !(existingMethod.isConstructor || method.isConstructor))
                throw new ConcreteClassAbstractMethodException(method, typeDeclaration)
            case _ =>
          }
        }
      })
  }

  override def apply(typeDeclaration: TypeDeclaration) = {
    // 1. The hierarchy must be acyclic.
    checkCyclicHierarchy(typeDeclaration)

    val inheritMethods = typeDeclaration.inheritedMethods
    val localMethods = typeDeclaration.methods

    checkReturnType(typeDeclaration)
    localMethods.map(
      method => {

        if (typeDeclaration.isConcreteClass && method.isAbstractMethod)
          throw new ConcreteClassAbstractMethodException(method, typeDeclaration)
      }
    )
    val localSignatures = localMethods.map(method => method.localSignature)
    for (method <- inheritMethods.values.flatten) {
      if (typeDeclaration.isConcreteClass &&
          method.isAbstractMethod &&
          !localSignatures.contains(method.localSignature))
        throw new ConcreteClassAbstractMethodException(method, typeDeclaration)
    }
    ensureValidReplaces(typeDeclaration)
    typeDeclaration.methods.foreach(_.accept(this))
    // A class or interface must not contain (declare or inherit) two methods with the same signature but different return types
    val dupe = findDuplicate(typeDeclaration.constructorMap.values.map(_.typedSignature))
    if (dupe.isDefined) {
      throw new SameMethodSignatureException(dupe.get, typeDeclaration)
    }
  }

  private[this] def ensureValidReplaces(typeDeclaration: TypeDeclaration)(implicit unit: CompilationUnit) {

    for (method <- typeDeclaration.methodMap.values) {
      for (inheritedMethod <- typeDeclaration.containedMethods.values.flatten) {
        if (method.typedSignature == inheritedMethod.typedSignature &&
            !areEqual(method.typeDeclaration, inheritedMethod.typeDeclaration)) {
          ensureValidReplaces(method, inheritedMethod)
        }
      }
    }

    for (inheritedMethodA <- typeDeclaration.containedMethods.values.flatten) {
      for (inheritedMethodB <- typeDeclaration.containedMethods.values.flatten) {
        if (!inheritedMethodA.isAbstractMethod && inheritedMethodB.isAbstractMethod) {
          if (inheritedMethodA.typedSignature == inheritedMethodB.typedSignature
              && !typeDeclaration.methodMap.contains(inheritedMethodA.typedSignature)) {
            ensureValidReplaces(inheritedMethodA, inheritedMethodB)
          }
        }
      }
    }
  }

  private[this] def ensureValidReplaces(method: MethodDeclaration, inheritedMethod: MethodDeclaration)(implicit unit: CompilationUnit) {
    if (areEqual(method.typeDeclaration, inheritedMethod.typeDeclaration)) {
      // If both methods are the same, we don't do check
      return
    }

    if (!method.modifiers.contains(Modifier.Static)
        && inheritedMethod.modifiers.contains(Modifier.Static)) {
      throw new OverrideStaticMethodException(method, inheritedMethod)
    }

    // A static method must not replace an instance method
    if (method.modifiers.contains(Modifier.Static)
        && !inheritedMethod.modifiers.contains(Modifier.Static)) {
      throw new OverrideStaticMethodException(method, inheritedMethod)
    }

    if (!areEqual(method.returnType, inheritedMethod.returnType)) {
      throw new OverrideReturnTypeException(method, inheritedMethod)
    }

    if (inheritedMethod.modifiers.contains(Modifier.Final)) {
      throw new OverrideFinalMethodException(method, inheritedMethod)
    }

    if ((method.modifiers contains Modifier.Protected) &&
        (inheritedMethod.modifiers contains Modifier.Public)) {
      throw new OverrideProtectedMethodException(method, inheritedMethod)
    }
  }

  override def apply(compilationUnit: CompilationUnit): Unit = {
    compilationUnit.typeDeclaration.map(_.accept(this))
  }
}

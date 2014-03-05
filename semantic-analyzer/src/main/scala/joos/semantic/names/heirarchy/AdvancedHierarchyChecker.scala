package joos.semantic.names.heirarchy

import joos.ast.declarations.{MethodDeclaration, TypeDeclaration, ModuleDeclaration}
import joos.ast.{Modifier, CompilationUnit, AstVisitor}
import joos.core.Logger
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
class AdvancedHierarchyChecker(implicit module: ModuleDeclaration) extends AstVisitor with TypeHierarchyChecker {
  private[this] implicit val typeDeclarations = mutable.Stack[TypeDeclaration]()
  private[this] val methodDeclarations = mutable.Stack[MethodDeclaration]()
  private[this] implicit var unit: CompilationUnit = null

  private def checkReturnType(methods: Seq[MethodDeclaration]) {
    var set: mutable.HashMap[String, MethodDeclaration] = mutable.HashMap()

    methods.foreach(
      method => {
        if (!set.contains(method.localSignature))
          set += {method.localSignature -> method}
        else {
          set.get(method.localSignature) match {
            case Some(existingMethod) =>
              if (!areEqual(existingMethod.returnType, method.returnType) && !(existingMethod.isConstructor || method.isConstructor))
                throw new ConcreteClassAbstractMethodException(method, typeDeclarations.top)
            case _ =>
          }
        }
      })
  }

  override def apply(typeDeclaration: TypeDeclaration) = {
    // 1. The hierarchy must be acyclic.
    typeDeclarations.push(typeDeclaration)

    val curTypeDeclaration = typeDeclarations.top
    val inheritMethods = curTypeDeclaration.inheritMethods
    val localMethods = curTypeDeclaration.methods

    checkReturnType(inheritMethods.values.flatten.toSeq ++ localMethods.toSeq)
    localMethods.map(
      method => {

        if (curTypeDeclaration.isConcreteClass && method.isAbstractMethod)
          throw new ConcreteClassAbstractMethodException(method, curTypeDeclaration)
      }
    )
    val localSignatures = localMethods.map(method => method.localSignature)
    for ((inherited, inheritedMethods) <- inheritMethods) {
      inheritedMethods.foreach(
        method => {
          if (curTypeDeclaration.isConcreteClass &&
              method.isAbstractMethod &&
              !localSignatures.contains(method.localSignature))
            throw new ConcreteClassAbstractMethodException(method, curTypeDeclaration)
        }
      )
    }
    ensureValidReplaces()
    typeDeclaration.methods.foreach(_.accept(this))
    typeDeclarations.pop
    // A class or interface must not contain (declare or inherit) two methods with the same signature but different return types
    val dupe = findDuplicate(typeDeclaration.constructorMap.values.toSeq.map(_.typedSignature))
    if (dupe.isDefined) {
      throw new SameMethodSignatureException(dupe.get, typeDeclaration)
    }
  }

  // A non-static method must not replace a static method.
  // A protected method must not replace a public method.
  // A method must not replace a final method.
  private def checkModifiers(childMethod: MethodDeclaration, parentMethod: MethodDeclaration) = {
    if (!(childMethod.modifiers contains Modifier.Static) &&
        (parentMethod.modifiers contains Modifier.Static)) {
      throw new OverrideStaticMethodException(childMethod, parentMethod)
    }
    if ((childMethod.modifiers contains Modifier.Protected) &&
        (parentMethod.modifiers contains Modifier.Public)) {
      throw new OverrideProtectedMethodException(childMethod, parentMethod)
    }
    if (parentMethod.modifiers contains Modifier.Final) {
      throw new OverrideFinalMethodException(childMethod, parentMethod)
    }
  }

  // A method must not replace a method with a different return type.
  private def checkReturnType(childMethod: MethodDeclaration, parentMethod: MethodDeclaration) {
    implicit val unit = childMethod.compilationUnit
    (childMethod.returnType, parentMethod.returnType) match {
      case (None, Some(_)) | (Some(_), None) =>
        throw new OverrideReturnTypeException(childMethod, parentMethod)
      case (Some(childRT), Some(parentRT)) => {
        if (!areEqual(childRT, parentRT)) {
          throw new OverrideReturnTypeException(childMethod, parentMethod)
        }
      }
      case _ =>
    }
  }

  private[this] def ensureValidReplaces()(implicit unit: CompilationUnit) {
    val typeDeclaration = typeDeclarations.top

    for ((_, method) <- typeDeclaration.methodMap) {
      for ((_, inheritedMethods) <- typeDeclaration.containedMethodMap) {
        for (inheritedMethod <- inheritedMethods) {
//          println(inheritedMethod.name.identifier + " up")
          if (method.typedSignature == inheritedMethod.typedSignature &&
              !areEqual(method.typeDeclaration, inheritedMethod.typeDeclaration)) {
            ensureValidReplaces(method, inheritedMethod)
          }
        }
      }
    }

    for ((_, inheritedMethodsA) <- typeDeclaration.containedMethodMap) {
      for (inheritedMethodA <- inheritedMethodsA) {
        for ((_, inheritedMethodsB) <- typeDeclaration.containedMethodMap) {
          for (inheritedMethodB <- inheritedMethodsB) {
            if (!inheritedMethodA.isAbstractMethod && inheritedMethodB.isAbstractMethod) {
              if (inheritedMethodA.typedSignature == inheritedMethodB.typedSignature
                  && !typeDeclaration.methodMap.contains(inheritedMethodA.typedSignature)) {
                ensureValidReplaces(inheritedMethodA, inheritedMethodB)
              }
            }
          }
        }
      }
    }
  }

  private[this] def ensureValidReplaces(method: MethodDeclaration, inheritedMethod: MethodDeclaration)(implicit unit: CompilationUnit) {
//    if (method.typeDeclaration eq inheritedMethod.typeDeclaration) {
//      // If both methods are the same, we don't do check
//      return
//    }

    if (areEqual(method.typeDeclaration, inheritedMethod.typeDeclaration)) {
      // If both methods are the same, we don't do check
      return
    }

    Logger.logInformation(s"Checking methods ${method} AND ${inheritedMethod}")

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

    if (!method.modifiers.contains(Modifier.Public)
        && inheritedMethod.modifiers.contains(Modifier.Public)) {
      throw new SemanticException(s"The declared method ${method.name} should have 'public' modifier")
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
    this.unit = compilationUnit
    compilationUnit.typeDeclaration.map(_.accept(this))
  }
}

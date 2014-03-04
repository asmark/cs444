package joos.semantic.names.heirarchy

import joos.ast.{CompilationUnit, Modifier, AstVisitor}
import joos.ast.declarations.{MethodDeclaration, TypeDeclaration, ModuleDeclaration}
import scala.collection.mutable
import joos.semantic.EnvironmentComparisons
import joos.core.Logger

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
  // This function also stores the parent classes and interfaces of the hierarchy in the environment of each type declaration
  private def checkCyclic(curTypeDeclaration: TypeDeclaration) = {
        var visited: Set[TypeDeclaration] = Set()

        val ancestors = mutable.Queue[TypeDeclaration]()
        ancestors enqueue curTypeDeclaration

        while (!ancestors.isEmpty) {
          val front = ancestors.dequeue()

          visited += front

          front.superType match {
            case Some(nameExpression) =>
              curTypeDeclaration.compilationUnit.getVisibleType(nameExpression) match {
                case Some(ancestor) => {
                  // Check
                  if (ancestor.equals(curTypeDeclaration))
                    throw new CyclicHierarchyException(ancestor.name)
                  if (!visited.contains(ancestor))
                    ancestors enqueue ancestor
                }
                case _ => Logger.logError(s"Parent type ${nameExpression.standardName} not visible to child type ${front.name.standardName}")
              }
            case _ =>
          }
          front.superInterfaces.foreach(implmented =>
            curTypeDeclaration.compilationUnit.getVisibleType(implmented) match {
              case Some(ancestor) => {
                // Check
                if (ancestor.equals(curTypeDeclaration))
                  throw new CyclicHierarchyException(ancestor.name)
                if (!visited.contains(ancestor))
                  ancestors enqueue ancestor
              }
              case _ => Logger.logError(s"Interface ${implmented.standardName} not visible to implementer ${front.name.standardName}")
            }
          )
        }
  }

  // A non-static method must not replace a static method.
  // A protected method must not replace a public method.
  // A method must not replace a final method.
  private def checkModifiers(childMethod: MethodDeclaration, parentMethod: MethodDeclaration) = {
    if (!EnvironmentComparisons.containsModifier(childMethod.modifiers, Modifier.Static) &&
        EnvironmentComparisons.containsModifier(parentMethod.modifiers, Modifier.Static)) {
      throw new OverrideStaticMethodException(childMethod, parentMethod)
    }
    if (EnvironmentComparisons.containsModifier(childMethod.modifiers, Modifier.Protected) &&
        EnvironmentComparisons.containsModifier(parentMethod.modifiers, Modifier.Public)) {
      throw new OverrideProtectedMethodException(childMethod, parentMethod)
    }
    if (EnvironmentComparisons.containsModifier(parentMethod.modifiers, Modifier.Final)) {
      throw new OverrideFinalMethodException(childMethod, parentMethod)
    }
  }

  // A method must not replace a method with a different return type.
  private def checkReturnType(childMethod: MethodDeclaration, parentMethod: MethodDeclaration) = {
    (childMethod.returnType, parentMethod.returnType) match {
      case (None, None) => {} // TODO: Set up special void return type?
      case (None, Some(_)) | (Some(_), None) =>
        throw new OverrideReturnTypeException(childMethod, parentMethod)
      case (Some(childRT), Some(parentRT)) => {
        // TODO: the following code doesn't work
//          if (!childRT.asName.standardName.equals(parentRT.asName.standardName))
//            throw new OverrideReturnTypeException(childMethod, parentMethod)
      }
    }
  }

  private def checkMethodReplaces(curMethodDeclaration: MethodDeclaration) = {
    val curTypeDeclaration = curMethodDeclaration.typeDeclaration

    var visited: Set[TypeDeclaration] = Set()

    val ancestors = mutable.Queue[TypeDeclaration]()
    ancestors enqueue curTypeDeclaration

    while (!ancestors.isEmpty) {
      implicit val front = ancestors.dequeue()

      visited += front

      front.superType match {
        case Some(superType) => {
          superType.methods.foreach(method =>
            if (method.localSignature.equals(curMethodDeclaration.localSignature)) {
              checkModifiers(curMethodDeclaration, method)
              checkReturnType(curMethodDeclaration, method)
            }
          )

          if (!visited.contains(superType))
            ancestors enqueue superType
        }
        case _ =>
      }
      val interfaces = front.superInterfaces
      for (interface <- interfaces) {
        interface.methods.foreach(method =>
          if (method.localSignature.equals(curMethodDeclaration.localSignature)) {
            checkModifiers(curMethodDeclaration, method)
            checkReturnType(curMethodDeclaration, method)
          }
        )

        if (!visited.contains(interface))
          ancestors enqueue interface
      }
    }
  }

  override def apply(typeDeclaration: TypeDeclaration) = {
    // 1. The hierarchy must be acyclic.
    checkCyclic(typeDeclaration)
    // A class that contains (declares or inherits) any abstract methods must be abstract.
    val isAbstractType: Boolean =
      EnvironmentComparisons.containsModifier(typeDeclaration.modifiers, Modifier.Abstract)
    // A class or interface must not declare two methods with the same signature (name and parameter types).
    val localMethods: mutable.HashSet[String] = mutable.HashSet()

    typeDeclaration.methods.foreach(
      method => {
        if (!isAbstractType && EnvironmentComparisons.containsModifier(method.modifiers, Modifier.Abstract))
          throw new ConcreteClassAbstractMethodException(method, typeDeclaration)

        if (localMethods.contains(method.localSignature)) {
          throw new SameMethodSignatureException(method, typeDeclaration)
        } else {
          localMethods.add(method.localSignature)
        }

        method.accept(this)
      }
    )
  }

  override def apply(methodDeclaration: MethodDeclaration) = {
    checkMethodReplaces(methodDeclaration)
  }

  override def apply(compilationUnit: CompilationUnit): Unit = {
    compilationUnit.typeDeclaration.map(_.accept(this))
  }
}

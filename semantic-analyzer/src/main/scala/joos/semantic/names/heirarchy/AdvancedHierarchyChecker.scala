package joos.semantic.names.heirarchy

import joos.ast.declarations.{MethodDeclaration, TypeDeclaration, ModuleDeclaration}
import joos.ast.{CompilationUnit, Modifier, AstVisitor}
import joos.core.Logger
import joos.semantic.{SemanticException, EnvironmentComparisons}
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
  private[this] implicit val typeDeclarations = mutable.Stack[TypeDeclaration]()
  private[this] val methodDeclarations = mutable.Stack[MethodDeclaration]()

  // This function also stores the parent classes and interfaces of the hierarchy in the environment of each type declaration
  private def checkCyclic() = {
    val curTypeDeclaration = typeDeclarations.top

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
      front.superInterfaces.foreach(
        implmented =>
          curTypeDeclaration.compilationUnit.getVisibleType(implmented) match {
            case Some(ancestor) => {
              // Check
              if (ancestor.equals(curTypeDeclaration))
                throw new CyclicHierarchyException(ancestor.name)
              if (!visited.contains(ancestor))
                ancestors enqueue ancestor
            }
              // TODO: This case is wrong
            case _ => Logger.logError(s"Interface ${implmented.standardName} not visible to implementer ${front.name.standardName}")
          }
      )
    }
  }

  override def apply(typeDeclaration: TypeDeclaration) = {
    // 1. The hierarchy must be acyclic.
    typeDeclarations.push(typeDeclaration)
    checkCyclic()
    typeDeclaration.methods.foreach(_.accept(this))
    typeDeclarations.pop

    // A class or interface must not contain (declare or inherit) two methods with the same signature but different return types
    val dupe = EnvironmentComparisons.findDuplicate(typeDeclaration.getConstructors.toSeq.map(_.typedSignature))
    if (dupe.isDefined) {
      throw new SemanticException(s"found duplicate ${dupe.get}")
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
  private def checkReturnType(childMethod: MethodDeclaration, parentMethod: MethodDeclaration) = {
    (childMethod.returnType, parentMethod.returnType) match {
      case (None, None) => {} // TODO: Set up special void return type?
      case (None, Some(_)) | (Some(_), None) =>
        throw new OverrideReturnTypeException(childMethod, parentMethod)
      case (Some(childRT), Some(parentRT)) => {
        // TODO: the following code doesn't work
        typeEquality(childRT, parentRT)(unit)
      }
    }
  }

  private def checkMethodReplaces() = {
    val curTypeDeclaration = typeDeclarations.top
    val curMethodDeclaration = methodDeclarations.top

    var visited: Set[TypeDeclaration] = Set()

    val ancestors = mutable.Queue[TypeDeclaration]()
    ancestors enqueue curTypeDeclaration

    while (!ancestors.isEmpty) {
      implicit val front = ancestors.dequeue()

      visited += front

      front.superType match {
        case Some(superType) => {
          superType.methods.foreach(
            method =>
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
        interface.methods.foreach(
          method =>
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

  override def apply(methodDeclaration: MethodDeclaration) = {
    methodDeclarations.push(methodDeclaration)
    checkMethodReplaces()
    methodDeclarations.pop
  }

  override def apply(compilationUnit: CompilationUnit): Unit = {
    compilationUnit.typeDeclaration.map(_.accept(this))
  }
}

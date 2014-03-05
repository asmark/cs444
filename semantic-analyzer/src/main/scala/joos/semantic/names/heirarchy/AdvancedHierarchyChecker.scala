package joos.semantic.names.heirarchy

import joos.ast.declarations.{MethodDeclaration, TypeDeclaration, ModuleDeclaration}
import joos.ast.{Modifier, CompilationUnit, AstVisitor}
import joos.core.Logger
import scala.collection.mutable
import joos.semantic._
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

  // This function also stores the parent classes and interfaces of the hierarchy in the environment of each type declaration
  private def checkCyclic() = {
    val curTypeDeclaration = typeDeclarations.top

    var visited: Set[TypeDeclaration] = Set()

    val ancestors = mutable.Queue[TypeDeclaration](curTypeDeclaration)

    while (!ancestors.isEmpty) {
      val front = ancestors.dequeue()

      visited += front

      getSuperType(front) match {
        case Some(ancestor) => {
          // Check
          if (ancestor.equals(curTypeDeclaration))
            throw new CyclicHierarchyException(ancestor.name)
          if (!visited.contains(ancestor))
            ancestors enqueue ancestor
        }
        case None =>
      }

      front.superInterfaces.foreach {
        implemented =>
          front.compilationUnit.getVisibleType(implemented) match {
            case Some(ancestor) => {
              // Check
              if (ancestor.equals(curTypeDeclaration))
                throw new CyclicHierarchyException(ancestor.name)
              if (!visited.contains(ancestor))
                ancestors enqueue ancestor
            }
            // TODO: This case is wrong
            case _ => Logger.logError(s"Interface ${implemented.standardName} not visible to implementer ${front.name.standardName}")
          }
      }
    }
  }


  override def apply(typeDeclaration: TypeDeclaration) = {
    // 1. The hierarchy must be acyclic.
    typeDeclarations.push(typeDeclaration)
    checkCyclic()
    typeDeclaration.methods.foreach(_.accept(this))
    typeDeclarations.pop
    // A class or interface must not contain (declare or inherit) two methods with the same signature but different return types
    val dupe = findDuplicate(typeDeclaration.constructors.toSeq.map(_.typedSignature))
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
  private def checkReturnType(childMethod: MethodDeclaration, parentMethod: MethodDeclaration) = {
    implicit val unit = childMethod.compilationUnit
    (childMethod.returnType, parentMethod.returnType) match {
      case (None, None) => {
      } // TODO: Set up special void return type?
      case (None, Some(_)) | (Some(_), None) =>
        throw new OverrideReturnTypeException(childMethod, parentMethod)
      case (Some(childRT), Some(parentRT)) => {
        areEqual(childRT, parentRT)
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
      implicit val unit = front.compilationUnit

      visited += front

      getSuperType(front) match {
        case Some(ancestor) => {
          ancestor.methods.foreach(
            method =>
              if (method.localSignature.equals(curMethodDeclaration.localSignature)) {
                checkModifiers(curMethodDeclaration, method)
                checkReturnType(curMethodDeclaration, method)
              }
          )

          if (!visited.contains(ancestor))
            ancestors enqueue ancestor
        }
        case None =>
      }

      val interfaces = front.superInterfaces
      for (interface <- interfaces) {
        resolveType(interface).methods.foreach(
          method =>
            if (method.localSignature.equals(curMethodDeclaration.localSignature)) {
              checkModifiers(curMethodDeclaration, method)
              checkReturnType(curMethodDeclaration, method)
            }
        )

        if (!visited.contains(resolveType(interface)))
          ancestors enqueue resolveType(interface)
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

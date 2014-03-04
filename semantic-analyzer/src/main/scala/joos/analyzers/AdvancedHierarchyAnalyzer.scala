package joos.analyzers

import joos.ast.declarations.{MethodDeclaration, TypeDeclaration, ModuleDeclaration}
import joos.ast.{Modifier, CompilationUnit, AstVisitor}
import scala.collection.mutable
import joos.core.Logger
import joos.semantic.EnvironmentComparisons
import joos.ast.expressions.NameExpression

/*
 Semantic Checks
 1. The hierarchy must be acyclic.
 2. A class or interface must not declare two methods with the same signature (name and parameter types).
 3. A class or interface must not contain (declare or inherit) two methods with the same signature but different return types
 4. A class that contains (declares or inherits) any abstract methods must be abstract.
 5. A nonstatic method must not replace a static method
 6. A method must not replace a method with a different return type.
 7. A protected method must not replace a public method.
 8. A method must not replace a final method.
*/
class AdvancedHierarchyAnalyzer(implicit module: ModuleDeclaration) extends AstVisitor {
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
              // Augment parent class in the current type
              if (EnvironmentComparisons.typeEquality(front, curTypeDeclaration))
                // TODO: Take this out. We should not be initializing half of the TypeEnvironment here and half elsewhere
                front.add(ancestor)
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
            // Augment interface in the current type
            if (EnvironmentComparisons.typeEquality(front, curTypeDeclaration))
              front.add(ancestor)
          }
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
        if (!childRT.asName.standardName.equals(parentRT.asName.standardName))
          throw new OverrideReturnTypeException(childMethod, parentMethod)
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
      val front = ancestors.dequeue()

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
      val interfaces = front.getAllImplementedInterfaces
      for (interface <- interfaces) {
        val interfaceDeclaration = interface._2
        interfaceDeclaration.methods.foreach(method =>
          if (method.localSignature.equals(curMethodDeclaration.localSignature)) {
            checkModifiers(curMethodDeclaration, method)
            checkReturnType(curMethodDeclaration, method)
          }
        )

        if (!visited.contains(interfaceDeclaration))
          ancestors enqueue interfaceDeclaration
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

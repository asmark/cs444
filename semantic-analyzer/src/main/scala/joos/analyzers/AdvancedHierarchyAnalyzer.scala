package joos.analyzers

import joos.ast.declarations.{MethodDeclaration, TypeDeclaration, ModuleDeclaration}
import joos.ast.{Type, CompilationUnit, AstVisitor}
import scala.collection.mutable
import joos.tokens.TokenKind

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

    var ancestors = mutable.Queue[TypeDeclaration]()
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
                throw new SemanticAnalyzerException("The hierarchy must be acyclic.") // TODO: Create different exception
              if (!visited.contains(ancestor))
                ancestors += ancestor
              // Augment parent class in the current type
              front.add(ancestor)
            }
            case _ => // TODO: Log this case?
          }
        case _ => // TODO: Log this case?
      }
      front.superInterfaces.foreach(interface =>
        curTypeDeclaration.compilationUnit.getVisibleType(interface) match {
          case Some(ancestor) => {
            // Check
            if (ancestor.equals(curTypeDeclaration))
              throw new SemanticAnalyzerException("The hierarchy must be acyclic.") // TODO: Create different exception
            if (!visited.contains(ancestor))
              ancestors += ancestor
            // Augment interface in the current type
            front.add(ancestor)
          }
          case _ => // TODO: Log this case?
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

  // A nonstatic method must not replace a static method.
  // A protected method must not replace a public method.
  // A method must not replace a final method.
  // TODO: Use EnvironmentComparisons.containsModifier
  private def checkModifiers(childMethod: MethodDeclaration, parentMethod: MethodDeclaration) = {
    val childModifiers = childMethod.modifiers.map(modifier =>
      modifier.modifier.kind
    )
    val parentModifiers = parentMethod.modifiers.map(modifier =>
      modifier.modifier.kind
    )

    if (!childModifiers.contains(TokenKind.Static) && parentModifiers.contains(TokenKind.Static)) {
      // TODO: Create different exception
      throw new SemanticAnalyzerException(
        s"A nonstatic method must not replace a static method: ${childMethod.typedSignature} and ${parentMethod.typedSignature}"
      )
    }
    if (childModifiers.contains(TokenKind.Protected) && parentModifiers.contains(TokenKind.Public)) {
      // TODO: Create different exception
      throw new SemanticAnalyzerException(
        s"A protected method must not replace a public method: ${childMethod.typedSignature} and ${parentMethod.typedSignature}"
      )
    }
    if (parentModifiers.contains(TokenKind.Final)) {
      // TODO: Create different exception
      throw new SemanticAnalyzerException(
        s"A method must not replace a final method: ${childMethod.typedSignature} and ${parentMethod.typedSignature}"
      )
    }
  }

  // A method must not replace a method with a different return type.
  private def checkReturnType(childMethod: MethodDeclaration, parentMethod: MethodDeclaration) = {
    (childMethod.returnType, parentMethod.returnType) match {
      case (None, None) => {} // TODO: Log this case?
      case (None, Some(_)) | (Some(_), None) =>
        // TODO: Create different exception
        throw new SemanticAnalyzerException(
          s"A method must not replace a method with a different return type: ${childMethod.typedSignature} and ${parentMethod.typedSignature}"
        )
      case (Some(childRT), Some(parentRT)) => {
        if (!childRT.asName.standardName.equals(parentRT.asName.standardName))
        // TODO: Create different exception
          throw new SemanticAnalyzerException(
            s"A method must not replace a method with a different return type: ${childMethod.typedSignature} and ${parentMethod.typedSignature}"
          )
      }
    }
  }
  
  private def checkMethodReplaces() = {
    val curTypeDeclaration = typeDeclarations.top
    val curMethodDeclaration = methodDeclarations.top

    var visited: Set[TypeDeclaration] = Set()

    var ancestors = mutable.Queue[TypeDeclaration]()
    ancestors enqueue curTypeDeclaration

    while (!ancestors.isEmpty) {
      val front = ancestors.dequeue()

      visited += front

      front.getExtendedClass match {
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

  override def apply(methodDeclaration: MethodDeclaration) = {
    methodDeclarations.push(methodDeclaration)
    checkMethodReplaces()
    methodDeclarations.pop
  }

  override def apply(compilationUnit: CompilationUnit): Unit = {
    compilationUnit.typeDeclaration.map(_.accept(this))
  }
}

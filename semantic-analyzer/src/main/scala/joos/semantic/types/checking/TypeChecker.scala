package joos.semantic.types.checking

import joos.ast.declarations.{TypeDeclaration, MethodDeclaration, FieldDeclaration}
import joos.ast.visitor.AstCompleteVisitor
import joos.ast.{Modifier, CompilationUnit}
import joos.semantic._
import joos.semantic.types._

class TypeChecker(implicit val unit: CompilationUnit) extends AstEnvironmentVisitor
with AssignmentExpressionTypeChecker
with ArrayAccessExpressionTypeChecker
with ArrayCreationExpressionTypeChecker
with CastExpressionTypeChecker
with ClassInstanceCreationExpressionTypeChecker
with FieldAccessExpressionTypeChecker
with InfixExpressionTypeChecker
with MethodInvocationExpressionTypeChecker
with ParenthesizedExpressionTypeChecker
with ThisExpressionTypeChecker
with VariableDeclarationExpressionTypeChecker {
  var checkImplicitThis = false // TODO: this approach should be fine as long as the field decl checking and method decl checking are not recursive

  override def apply(fieldDeclaration: FieldDeclaration) {
    // Check that the implicit this variable is not accessed in a static method or in the initializer of a static field.
    if (fieldDeclaration.modifiers.contains(Modifier.Static)) {
      checkImplicitThis = true
      try {
        super.apply(fieldDeclaration)
      } catch {
        case e: ImplicitThisInStaticException =>
          throw new ImplicitThisInStaticException(s"In ${fieldDeclaration.variableType.standardName}")
        case e: Throwable => throw e
      }
      checkImplicitThis = false
    }

  }

  override def apply(methodDeclaration: MethodDeclaration) {
    // Check that the implicit this variable is not accessed in a static method or in the initializer of a static field.
    if (methodDeclaration.modifiers.contains(Modifier.Static)) {
      checkImplicitThis = true
      try {
        super.apply(methodDeclaration)
      } catch {
        case e: ImplicitThisInStaticException =>
          throw new ImplicitThisInStaticException(s"In ${methodDeclaration.localSignature}")
        case e: Throwable => throw e
      }
      checkImplicitThis = false
    } else {
      super.apply(methodDeclaration)
    }
  }

  override def apply(typeDeclaration: TypeDeclaration) {
    // A constructor in a class other than java.lang.Object implicitly calls the zero-argument constructor of its superclass.
    // Check that this zero-argument constructor exists.
    getSuperType(typeDeclaration) map {
      superType => {
        val zeroArgConstructor = superType.constructorMap.values.find(_.parameters.size == 0)
        if (zeroArgConstructor.isEmpty)
          throw new MissingConstructorException(s"Missing zero argument constructor in ${superType.declarationName.standardName}")
      }
    }

    typeDeclaration.constructorMap.values.foreach {
      constructor =>
        if (!(constructor.name equals typeDeclaration.name)) {
          throw new InvalidConstructorException(s"Mismatched constructor and type declaration ${typeDeclaration.declarationName.standardName}")
        }
    }
    super.apply(typeDeclaration)
  }
}

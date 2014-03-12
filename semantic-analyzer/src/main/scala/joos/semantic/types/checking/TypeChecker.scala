package joos.semantic.types.checking

import joos.ast.visitor.AstCompleteVisitor
import joos.ast.{Modifier, CompilationUnit}
import joos.ast.declarations.{TypeDeclaration, MethodDeclaration, FieldDeclaration}
import joos.ast.expressions._
import joos.semantic.types._
import joos.ast.types.{ArrayType, SimpleType, PrimitiveType, Type}
import joos.syntax.tokens.{TokenKind, TerminalToken}
import scala.Some
import joos.syntax.tokens.TerminalToken

class TypeChecker(implicit val unit: CompilationUnit) extends AstCompleteVisitor
  with AssignmentExpressionTypeChecker
  with ArrayAccessExpressionTypeChecker
  with ArrayCreationExpressionTypeChecker
  with CastExpressionTypeChecker
  with ClassCreationExpressionTypeChecker
  with FieldAccessExpressionTypeChecker
  with InfixExpressionTypeChecker
  with MethodInvocationExpressionTypeChecker
  with ParenthesizedExpressionTypeChecker
  with QualifiedNameExpressionTypeChecker
  with SimpleNameExpressionTypeChecker
  with ThisExpressionTypeChecker
  with VariableDeclarationExpressionTypeChecker {
  var checkImplicitThis = false // TODO: this approach should be fine as long as the field decl checking and method decl checking are not recursive

  override def apply(fieldDeclaration: FieldDeclaration) {
    // Check that the implicit this variable is not accessed in a static method or in the initializer of a static field.
    if (fieldDeclaration.modifiers.contains(Modifier.Static)) {
      checkImplicitThis = true
      try {
        super.apply(fieldDeclaration)
      } catch  {
        case e: ImplicitThisInStaticExpception =>
          throw new ImplicitThisInStaticExpception(s"In ${fieldDeclaration.variableType.standardName}")
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
      } catch  {
        case e: ImplicitThisInStaticExpception =>
          throw new ImplicitThisInStaticExpception(s"In ${methodDeclaration.localSignature}")
        case e: Throwable => throw e
      }
      checkImplicitThis = false
    } else {
      super.apply(methodDeclaration)
    }

  }
}

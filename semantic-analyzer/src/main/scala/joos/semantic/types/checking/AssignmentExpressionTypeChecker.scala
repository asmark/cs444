package joos.semantic.types.checking

import joos.ast.DeclarationReference
import joos.ast.declarations.{SingleVariableDeclaration, FieldDeclaration}
import joos.ast.expressions.{ArrayAccessExpression, VariableDeclarationExpression, AssignmentExpression}
import joos.ast.types.ArrayType
import joos.ast.visitor.AstVisitor
import joos.semantic._
import joos.semantic.types.AssignmentExpressionException

trait AssignmentExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(assignment: AssignmentExpression) {
    val left = assignment.left
    val right = assignment.right

    left.accept(this)
    right.accept(this)

    require(left.expressionType != null)
    require(right.expressionType != null)

    val leftType = left.expressionType
    val rightType = right.expressionType
    // TODO: Double check if the following are complete (probably not)
    if (isAssignable(leftType, rightType)) {
      assignment.expressionType = leftType
    } else {
      throw new AssignmentExpressionException(s"${rightType.standardName} to ${leftType.standardName}")
    }

    // We also need to make sure the left-hand side is a non-final variable/field reference
    left match {
      case reference: DeclarationReference[_] =>
        reference.declaration match {
          case variable: FieldDeclaration =>
            if (variable eq ArrayType.Length)
              throw new AssignmentExpressionException(left, right)
          case _: SingleVariableDeclaration =>
          case _: VariableDeclarationExpression =>
          case _ => throw new AssignmentExpressionException(left, right)
        }
      case _: ArrayAccessExpression =>
      case _ => throw new AssignmentExpressionException(left, right)
    }
  }
}


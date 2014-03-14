package joos.semantic.types.disambiguation

import joos.ast.compositions.NameLike
import joos.ast.declarations.{FieldDeclaration, TypeDeclaration}
import joos.ast.expressions._
import joos.ast.types.{PrimitiveType, SimpleType, ArrayType, Type}
import joos.ast.visitor.{AstCompleteVisitor, AstEnvironmentVisitor}
import joos.ast.{Modifier, CompilationUnit}
import joos.semantic.Declaration

// Check the rules specified in Section 8.3.2.3 of the Java Language Specification regarding forward references. The initializer of a non-static
// field must not use (i.e. read) by simple name (i.e. without an explicit this) itself or a non-static field declared later in the same class.
class ForwardUseChecker(fieldScope: Map[SimpleNameExpression, Type]) extends AstCompleteVisitor {

  override def apply(expression: FieldAccessExpression) {
    expression.expression match {
      case t: ThisExpression => return // No uses-before-declaration can occur in a this expression
      case _ => {
        expression.expression.accept(this)
        expression.identifier.accept(this)
      }
    }
  }

  override def apply(fieldName: SimpleNameExpression) {
    if (!(fieldScope contains fieldName)) {
      throw new ForwardFieldUseException(fieldName)
    }
  }

  override def apply(fieldAccess: QualifiedNameExpression) {
    if (fieldAccess.qualifier.classification == NameLike.ExpressionName) {
      fieldAccess.qualifier.accept(this)
    }
  }

  override def apply(assignment: AssignmentExpression) {
    assignment.right.accept(this)

    assignment.left match {
      case name: SimpleNameExpression => return // No uses-before-declaration can occur as a simple name on the left hand side of an assignment
      case complex => complex.accept(this)
    }
  }
}


class StaticNameLinker(implicit unit: CompilationUnit) extends AstEnvironmentVisitor {
  private var localFields = Map.empty[SimpleNameExpression, Type]

  override def apply(fieldDeclaration: FieldDeclaration) {
    fieldDeclaration.fragment.initializer foreach (_.accept(new ForwardUseChecker(localFields)))
    if (!(fieldDeclaration.modifiers contains Modifier.Static)) {
      localFields += (fieldDeclaration.declarationName -> fieldDeclaration.declarationType)
    }
    fieldDeclaration.fragment.initializer foreach (_.accept(this))
  }

  override def apply(invocation: MethodInvocationExpression) {
    invocation.expression foreach (_.accept(this))
    invocation.arguments foreach (_.accept(this))

    // Can't resolve method names yet
    //    invocation.methodName match {
    //      case QualifiedNameExpression(qualifier, methodName) => {
    //        qualifier.accept(this)
    //
    //        qualifier.declarationRef match {
    //          case Right(t@TypeDeclaration(_, _, _, _, _, _, _)) => {
    //            t.containedMethods.get(methodName) match {
    //              case Some(method) => {
    //                // TODO: Find correct method and ensure static
    //                invocation.methodName.declarationRef = Right(method.head)
    //              }
    //              case None => throw new AmbiguousNameException(invocation.methodName)
    //            }
    //          }
    //          case _ => throw new AmbiguousNameException(invocation.methodName)
    //        }
    //      }

    //      case methodName: SimpleNameExpression => {
    //        typeEnvironment.containedMethods.get(methodName) match {
    //          case Some(method) => {
    //            // TODO: Find correct method and ensure static
    //            invocation.methodName.declarationRef = Right(method.head)
    //          }
    //          case None => throw new AmbiguousNameException(invocation.methodName)
    //        }
    //
    //      }
  }

  //  override def apply(name: SimpleNameExpression) {
  //    var declaration: Declaration = null
  //
  //    // (1) Check local variable
  //    require(blockEnvironment != null)
  //    blockEnvironment.getVariable(name) match {
  //      case Some(localVariable) => declaration = getDeclarationRef(localVariable.declarationType)
  //      case None =>
  //
  //        // (2) Check local field
  //        typeEnvironment.containedFields.get(name) match {
  //          case Some(field) => {
  //            declaration = getDeclarationRef(field.declarationType)
  //            if (field.isStatic) {
  //              throw new InvalidStaticUseException(name)
  //            }
  //          }
  //          case None => {
  //
  //            // (3) Check Static access
  //            unit.getVisibleType(name) match {
  //              case Some(typeName) => {
  //                declaration = Right(typeName)
  //              }
  //              case None => throw new AmbiguousNameException(name)
  //            }
  //          }
  //        }
  //    }
  //    name.declarationRef = declaration
  //  }

  override def apply(name: QualifiedNameExpression) {
    var names = name.unfold
    var typeIndex = 1
    var declaration: Declaration = null


    // (1) Check local variable
    require(blockEnvironment != null)
    blockEnvironment.getVariable(names.head) match {
      case Some(localVariable) => declaration = getDeclarationRef(localVariable.declarationType)
      case None =>

        // (2) Check local field
        typeEnvironment.containedFields.get(names.head) match {
          case Some(field) => {
            declaration = getDeclarationRef(field.declarationType)
            if (field.isStatic) {
              throw new InvalidStaticUseException(name)
            }
          }
          case None => {

            // (3) Check static accesses

            // Must have a prefix that is a valid type
            while (unit.getVisibleType(names.take(typeIndex)).isEmpty) {
              typeIndex += 1
              if (typeIndex > names.length) {
                throw new AmbiguousNameException(name)
              }
            }

            val typeName = unit.getVisibleType(names.take(typeIndex)).get
            declaration = getDeclarationRef(typeName)

            // Next name must be a static field

            if (names.size > typeIndex) {
              val fieldName = names(typeIndex)
              typeName.containedFields.get(fieldName) match {
                case Some(field) => {
                  if (!field.isStatic) {
                    throw new InvalidStaticUseException(name)
                  }
                  declaration = getDeclarationRef(field.declarationType)(typeName.compilationUnit)
                  typeIndex += 1
                }
                case None => throw new AmbiguousNameException(name)
              }
            }

          }
        }
    }
    // All remaining names must be instance field accesses
    names = names.drop(typeIndex)
    names foreach {
      name =>
        declaration match {
          // Arrays only have a "length" field
          case (_: ArrayType, _) => {
            if (name.standardName equals "length") {
              declaration = (PrimitiveType.IntegerType, None)
            } else {
              throw new AmbiguousNameException(name)
            }
          }
          // Primitives do not have any fields
          case (_: PrimitiveType, _) => {
            throw new AmbiguousNameException(name)
          }

          case (_: SimpleType, Some(t: TypeDeclaration)) => {
            t.containedFields.get(name) match {
              case None => throw new AmbiguousNameException(name)
              case Some(field) => {
                if (field.isStatic) {
                  throw new InvalidStaticUseException(name)
                }
                declaration = getDeclarationRef(field.declarationType)(t.compilationUnit)
              }
            }
          }
          case _ => throw new AmbiguousNameException(name)
        }
    }
    name.declarationRef = declaration
  }

}

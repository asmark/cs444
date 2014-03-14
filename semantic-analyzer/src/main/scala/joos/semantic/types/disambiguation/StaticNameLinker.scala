package joos.semantic.types.disambiguation

import joos.ast.CompilationUnit
import joos.ast.declarations.{TypeDeclaration, BodyDeclaration}
import joos.ast.expressions.{NameExpression, SimpleNameExpression, QualifiedNameExpression, MethodInvocationExpression}
import joos.ast.types.{PrimitiveType, SimpleType, ArrayType, Type}
import joos.ast.visitor.AstEnvironmentVisitor
import joos.semantic.Declaration

class StaticNameLinker(implicit unit: CompilationUnit) extends AstEnvironmentVisitor {

  private def getDeclarationRef(t: Type)(implicit unit: CompilationUnit): Declaration = {

    def getTypeDeclaration(t: Type): Option[BodyDeclaration] = {
      t match {
        case ArrayType(x, _) => getTypeDeclaration(x)
        case SimpleType(x) => unit.getVisibleType(x)
        case _: PrimitiveType => None
      }
    }

    t match {
      case _: ArrayType => Left(getTypeDeclaration(t))
      case _: PrimitiveType => Left(None)
      case _: SimpleType => Right(getTypeDeclaration(t).get)
    }
  }

  private def fold(names: Seq[SimpleNameExpression]) = {
    names.reduceLeft {
      (tree: NameExpression, name: SimpleNameExpression) =>
        QualifiedNameExpression(tree, name)
    }
  }

  override def apply(typed: TypeDeclaration) {
    typed.fields.withFilter(f => f.isStatic) foreach (_.accept(this))
    typed.methods.withFilter(m => m.isStatic) foreach (_.accept(this))
  }

  // TODO: Only visit statics

  override def apply(invocation: MethodInvocationExpression) {
    // TODO
  }

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
            while (unit.getVisibleType(fold(names.take(typeIndex))).isEmpty) {
              typeIndex += 1
              if (typeIndex > names.length) {
                throw new AmbiguousNameException(name)
              }
            }

            val typeName = unit.getVisibleType(fold(names.take(typeIndex))).get
            declaration = Right(typeName)

            typeIndex += 1
            // Next name must be a static field
            if (names.size > typeIndex) {
              val fieldName = names(typeIndex)
              typeName.containedFields.get(fieldName) match {
                case Some(field) => {
                  if (!field.isStatic) {
                    throw new InvalidStaticUseException(name)
                  }
                  name.declarationRef = getDeclarationRef(field.declarationType)(typeName.compilationUnit)
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
          case Left(arrayType) => {
            name match {
              case SimpleNameExpression(token) if (token.lexeme equals "length") => declaration = Left(None)
              case _ => throw new AmbiguousNameException(name)
            }
          }

          case Right(t@TypeDeclaration(_, _, _, _, _, _, _)) => {
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

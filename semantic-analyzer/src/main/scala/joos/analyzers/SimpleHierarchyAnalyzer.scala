package joos.analyzers
import joos.ast.declarations.{TypeDeclaration, ModuleDeclaration}
import joos.ast._
import joos.tokens.TokenKind
import scala.Some

class SimpleHierarchyAnalyzer(implicit module: ModuleDeclaration) extends AstVisitor {

  private def isSameType(left: Type, right: Type, typeDeclaration: TypeDeclaration): Boolean = {
    left match {
      case leftArrayType: ArrayType => {
        right match {
          case rightArrayType: ArrayType => {
            isSameType(leftArrayType.elementType, rightArrayType.elementType, typeDeclaration)
          }
          case _ => false
        }
      }
      case leftPrimitiveType: PrimitiveType => {
        right match {
          case rightPrimitiveType: PrimitiveType => {
            leftPrimitiveType.primType.kind == leftPrimitiveType.primType.kind
          }
          case _ => false
        }
      }
      case leftSimpleType: SimpleType => {
        right match {
          case rightSimpleType: SimpleType => {
            typeDeclaration.compilationUnit.getVisibleType(leftSimpleType.name) match {
              case Some(leftVisibleType) => {
                typeDeclaration.compilationUnit.getVisibleType(rightSimpleType.name) match {
                  case Some(rightVisibleType) => {
                    leftVisibleType.eq(rightVisibleType)
                  }
                  case _ => false
                }
              }
              case _ => false
            }
          }
          case _ => false
        }
      }
      case _ => false
    }
  }

  override def apply(typeDeclaration: TypeDeclaration) = {
    typeDeclaration.isInterface match {
      case true => {
        var qualifiedInterfaces = typeDeclaration.superInterfaces.map(interface =>
          typeDeclaration.compilationUnit.getVisibleType(interface) match {
            case Some(aType) => {
              aType.isInterface match {
                case false => throw new SemanticAnalyzerException("A interface must not extend an class.")
                case true => Some(aType)
              }
            }
            case _ => None
          }
        )

        var isTypeMatch = true

        qualifiedInterfaces = qualifiedInterfaces.toIndexedSeq
        for (i <- 0 until qualifiedInterfaces.length) {
          for (j <- i+1 until qualifiedInterfaces.length) {
            isTypeMatch &= (qualifiedInterfaces(i) == qualifiedInterfaces(j))
          }
        }

        if (isTypeMatch)
          throw new SemanticAnalyzerException("An interface must not be repeated in an extends clause of an interface.")
      }
      case false => {
        // A class must not extend an interface.
        typeDeclaration.superType match {
          case Some(clazz) => {
            typeDeclaration.compilationUnit.getVisibleType(clazz) match {
              case Some(aType) => {
                aType.isInterface match {
                  case true =>
                    throw new SemanticAnalyzerException("A class must not extend an interface.")
                  case false => {
                    aType.modifiers.foreach(modifier =>
                      if (modifier.modifier.kind == TokenKind.Final)
                        throw new SemanticAnalyzerException("A class must not extend a final class.")
                    )
                  }
                }
              }
              case _ =>
            }
          }
          case _ =>
        }
        // A class must not implement a class.
        var qualifiedInterfaces = typeDeclaration.superInterfaces.map(interface =>
          typeDeclaration.compilationUnit.getVisibleType(interface) match {
            case Some(aType) => {
              aType.isInterface match {
                case false => throw new SemanticAnalyzerException("A class must not implement a class.")
                case _ =>
              }
            }
            case _ =>
          }
        )

        var isTypeMatch = true

        qualifiedInterfaces = qualifiedInterfaces.toIndexedSeq

        for (i <- 0 until qualifiedInterfaces.length) {
          for (j <- i+1 until qualifiedInterfaces.length) {
            isTypeMatch &= (qualifiedInterfaces(i) == qualifiedInterfaces(j))
          }
        }

        if (isTypeMatch)
          throw new SemanticAnalyzerException("An interface must not be repeated in an implements clause.")
      }
    }

    val constructors = typeDeclaration.methods.filter(method => method.isConstructor).toIndexedSeq
    for (i <- 0 until constructors.length) {
      for (j <- i+1 until constructors.length) {
        val left = constructors(i).parameters
        val right = constructors(j).parameters
        var isTypeMatch = true

        if (left.length == right.length) {
          for(k <- 0 until left.length) {
            isTypeMatch &= isSameType(left(k).variableType, right(k).variableType, typeDeclaration)
          }
        }

        if (isTypeMatch)
          throw new SemanticAnalyzerException("A class must not declare two constructors with the same parameter types")
      }
    }
  }
}

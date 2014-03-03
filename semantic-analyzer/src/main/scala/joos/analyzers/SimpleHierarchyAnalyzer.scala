package joos.analyzers

import joos.ast._
import joos.ast.declarations.{PackageDeclaration, TypeDeclaration, ModuleDeclaration}
import joos.tokens.TokenKind
import scala.Some
import scala.collection.mutable
import joos.ast.expressions.NameExpression

/**
 * Completes the following checks
 * (0) Any referenced type must be within the namespace
 * (1) Class A extends B => B must be a class {{{ExtendClassError}}}
 * (2) Class C implements D => D must be an interface {{{ImplementInterfaceError}}}
 * (3) Class C implements E F => E cannot equal F {{{ImplementInterfaceError}}}
 * (4) Class A extends B => B must not be final {{{ExtendClassError}}}
 * (5) Class A has constructor X, Y => X, Y must have distinct parameter types {{{TODO}}}
 */
class SimpleHierarchyAnalyzer(implicit module: ModuleDeclaration) extends AstVisitor {

  private def ExtendClassError(extendedType: TypeDeclaration)(implicit typeDeclaration: TypeDeclaration) = {
    s"${typeDeclaration} extends ${extendedType} which is not a class, or is final"
  }

  private def ImplementInterfaceError(implementedType: TypeDeclaration)(implicit typeDeclaration: TypeDeclaration) = {
    s"${typeDeclaration} implements ${implementedType} which is not an interface or is already implemented"
  }

  private def getType(typeName: NameExpression)(implicit typeDeclaration: TypeDeclaration) = {
    typeDeclaration.compilationUnit.getVisibleType(typeName)
  }


  override def apply(unit: CompilationUnit) {
    unit.typeDeclaration.map(_.accept(this))
  }


  override def apply(typeDeclaration: TypeDeclaration) {
    typeDeclaration.isInterface match {
      case true => analyzeInterfaceDeclaration
      case false => analyzeClassDeclaration
    }
  }

  private def analyzeInterfaceDeclaration(implicit typeDeclaration: TypeDeclaration) {
    require(typeDeclaration.isInterface)
    typeDeclaration.superInterfaces map {
      interface =>
        getType(interface) match {
          case None => throw new InvalidTypeReferenceException(interface)
          case Some(interfaceDeclaration) => interfaceDeclaration
        }
    }
  }

  private def analyzeClassDeclaration(implicit typeDeclaration: TypeDeclaration) {

  }


    typeDeclaration.isInterface match {
      case true => {
        var qualifiedInterfaces: Set[TypeDeclaration] = Set()
        typeDeclaration.superInterfaces.map(
          interface =>
            typeDeclaration.compilationUnit.getVisibleType(interface) match {
              case Some(aType) => {
                aType.isInterface match {
                  case false => throw new SemanticAnalyzerException("A interface must not extend an class.")
                  case true => {
                    if (qualifiedInterfaces.contains(aType)) {
                      throw new SemanticAnalyzerException("An interface must not be repeated in an extends clause of an interface.")
                    } else {
                      qualifiedInterfaces += aType
                    }
                  }
                }
              }
              case _ => None
            }
        )
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
                    aType.modifiers.foreach(
                      modifier =>
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

        val qualifiedInterfaces = mutable.HashSet.empty[(PackageDeclaration, TypeDeclaration)]
        // A class must not implement a class.
        typeDeclaration.superInterfaces.foreach {
          interface =>
            typeDeclaration.compilationUnit.getVisibleType(interface) match {
              case Some(aType) => {
                aType.isInterface match {
                  case false => throw new SemanticAnalyzerException("A class must not implement a class.")
                  case true => {
                    val qualifiedInterface = (aType.packageDeclaration, aType)
                    if (qualifiedInterfaces.contains(qualifiedInterface)) {
                      throw new SemanticAnalyzerException("An interface must not be repeated in an implements clause.")
                    } else {
                      qualifiedInterfaces += qualifiedInterface
                    }
                  }
                }
              }
              case _ => // TODO: Log this or something.
            }
        }

      }
    }
  }
}

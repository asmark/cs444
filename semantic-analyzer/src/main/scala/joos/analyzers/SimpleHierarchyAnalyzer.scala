package joos.analyzers
import joos.ast.declarations.{PackageDeclaration, TypeDeclaration, ModuleDeclaration}
import joos.ast._
import joos.tokens.TokenKind
import scala.Some
import scala.collection.mutable

class SimpleHierarchyAnalyzer(implicit module: ModuleDeclaration) extends AstVisitor {

  override def apply(unit: CompilationUnit) {
    unit.typeDeclaration.map(_.accept(this))
  }

  override def apply(typeDeclaration: TypeDeclaration) = {
    typeDeclaration.isInterface match {
      case true => {
        var qualifiedInterfaces: Set[TypeDeclaration] = Set()
        typeDeclaration.superInterfaces.map(interface =>
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

        val qualifiedInterfaces = mutable.HashSet.empty[(PackageDeclaration, TypeDeclaration)]
        // A class must not implement a class.
        typeDeclaration.superInterfaces.foreach{interface =>
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

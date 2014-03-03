package joos.analyzers

import joos.ast._
import joos.ast.declarations.{PackageDeclaration, TypeDeclaration, ModuleDeclaration}
import joos.ast.expressions.NameExpression
import joos.semantic.EnvironmentComparisons
import scala.Some
import scala.collection.mutable

/**
 * Completes the following checks
 * (0) Any referenced type must be within the namespace
 * (1) Class A extends B => B must be a class
 * (2) Class C implements D => D must be an interface
 * (3) Class C implements E F => E cannot equal F
 * (4) Class A extends B => B must not be final
 * (5) Class A has constructor X, Y => X, Y must have distinct parameter types {{{TODO}}}
 */
class SimpleHierarchyAnalyzer(implicit module: ModuleDeclaration) extends AstVisitor {

  private def InvalidExtendedClass(extendedType: TypeDeclaration)(implicit typeDeclaration: TypeDeclaration) = {
    s"${typeDeclaration} extends ${extendedType} which is final"
  }

  private def InvalidExtendedType(extendedType: TypeDeclaration)(implicit typeDeclaration: TypeDeclaration) = {
    s"${typeDeclaration} extends ${extendedType} which is not a class"
  }

  private def InvalidImplementedType(implementedType: TypeDeclaration)(implicit typeDeclaration: TypeDeclaration) = {
    s"${typeDeclaration} implements ${implementedType} which is not an interface"
  }

  private def DuplicateImplementedInterface(implementedType: TypeDeclaration)(implicit typeDeclaration: TypeDeclaration) = {
    s"${typeDeclaration} implements ${implementedType} twice"
  }

  private def getType(typeName: NameExpression)(implicit typeDeclaration: TypeDeclaration) = {
    typeDeclaration.compilationUnit.getVisibleType(typeName)
  }


  override def apply(unit: CompilationUnit) {
    unit.typeDeclaration.map(_.accept(this))
  }


  override def apply(typeDeclaration: TypeDeclaration) {
    typeDeclaration.isInterface match {
      case true => analyzeInterfaceDeclaration(typeDeclaration)
      case false => analyzeClassDeclaration(typeDeclaration)
    }
  }

  private def analyzeInterfaceDeclaration(implicit typeDeclaration: TypeDeclaration) {
    require(typeDeclaration.isInterface)
    assume(typeDeclaration.superType.isEmpty)
    analyzeImplementedInterfaces(typeDeclaration.superInterfaces)

  }

  private def analyzeImplementedInterfaces(interfaceNames: Seq[NameExpression])(implicit typeDeclaration: TypeDeclaration) {
    val interfaceSet = mutable.HashSet.empty[(PackageDeclaration, TypeDeclaration)]
    interfaceNames foreach {
      interfaceName =>
        getType(interfaceName) match {
          case None => throw new InvalidTypeReferenceException(interfaceName)
          case Some(interface) => {
            if (!interface.isInterface) {
              throw new SimpleHierarchyException(InvalidImplementedType(interface))
            } else if (!interfaceSet.add(interface.packageDeclaration, interface)) {
              throw new SimpleHierarchyException(DuplicateImplementedInterface(interface))
            }
          }
        }
    }
  }

  // TODO: If None, it should extend java.lang.Object?
  private def analyzeExtendedClass(superTypeName: Option[NameExpression])(implicit typeDeclaration: TypeDeclaration) {
    if (superTypeName.isDefined) {
      getType(superTypeName.get) match {
        case None => throw new InvalidTypeReferenceException(superTypeName.get)
        case Some(superType) => {
          if (superType.isInterface) {
            throw new SimpleHierarchyException(InvalidExtendedType(superType))
          } else if (EnvironmentComparisons.containsModifier(superType.modifiers, Modifier.Final)) {
            throw new SimpleHierarchyException(InvalidExtendedClass(superType))
          }
        }
      }

    }
  }

  private def analyzeClassDeclaration(implicit typeDeclaration: TypeDeclaration) {
    require(!typeDeclaration.isInterface)
    analyzeExtendedClass(typeDeclaration.superType)
    analyzeImplementedInterfaces(typeDeclaration.superInterfaces)
  }

}

package joos.semantic.names.heirarchy

import joos.ast._
import joos.ast.declarations.{PackageDeclaration, TypeDeclaration, ModuleDeclaration}
import joos.ast.expressions.NameExpression
import scala.collection.mutable
import joos.semantic._
import scala.Some
import joos.core.Logger

/**
 * SimpleHierarchyChecker is responsible for the following name resolution checks:
 *
 * A class must not extend an interface.
 * A class must not implement a class.
 * An interface must not be repeated in an implements clause, or in an extends clause of an interface.
 * A class must not extend a final class.
 * An interface must not extend a class.
 * A class must not declare two constructors with the same parameter types
 */
class SimpleHierarchyChecker(implicit module: ModuleDeclaration, unit: CompilationUnit) extends AstVisitor with TypeHierarchyChecker {

  private def checkCyclic(typeDeclaration: TypeDeclaration) = {
    val curTypeDeclaration = typeDeclaration

    var visited: Set[TypeDeclaration] = Set()

    val ancestors = mutable.Queue[TypeDeclaration](curTypeDeclaration)

    while (!ancestors.isEmpty) {
      val front = ancestors.dequeue()

      visited += front

      getSuperType(front) match {
        case Some(ancestor) => {
          // Check
          if (ancestor.equals(curTypeDeclaration))
            throw new CyclicHierarchyException(ancestor.name)
          if (!visited.contains(ancestor)) {
            ancestors enqueue ancestor
          }
        }
        case None =>
      }

      front.superInterfaces.foreach {
        implemented =>
          front.compilationUnit.getVisibleType(implemented) match {
            case Some(ancestor) => {
              // Check
              if (ancestor.equals(curTypeDeclaration))
                throw new CyclicHierarchyException(ancestor.name)
              if (!visited.contains(ancestor)) {
                ancestors enqueue ancestor
              }
            }
            // TODO: This case is wrong
            case _ => Logger.logError(s"Interface ${implemented.standardName} not visible to implementer ${front.name.standardName}")
          }
      }
    }
  }

  override def apply(unit: CompilationUnit) {
    unit.typeDeclaration.map(_.accept(this))
  }

  override def apply(typeDeclaration: TypeDeclaration) {
    typeDeclaration.isInterface match {
      case true => analyzeInterfaceDeclaration(typeDeclaration)
      case false => analyzeClassDeclaration(typeDeclaration)
    }

    // Check methods and constructors
    for (method <- typeDeclaration.methods) {
      if (method.isConstructor) {
        if (typeDeclaration.constructorMap.contains(method.typedSignature)) {
          throw new SameMethodSignatureException(method.typedSignature, typeDeclaration)
        }
      } else {
        if (typeDeclaration.methodMap.contains(method.typedSignature)) {
          throw new SameMethodSignatureException(method.typedSignature, typeDeclaration)
        }
      }
      typeDeclaration.add(method)
    }

    checkCyclic(typeDeclaration)
  }

  private def analyzeInterfaceDeclaration(implicit typeDeclaration: TypeDeclaration) {
    require(typeDeclaration.isInterface)
    assume(typeDeclaration.superType.isEmpty)
    analyzeImplementedInterfaces(typeDeclaration.superInterfaces)

  }

  private def analyzeImplementedInterfaces(interfaceNames: Seq[NameExpression])(implicit typeDeclaration: TypeDeclaration) {
    val interfaceSet = mutable.HashSet.empty[(PackageDeclaration, TypeDeclaration)]
    interfaceNames foreach {
      interface =>
        if (!interface.isInterface) {
          throw new InvalidImplementedTypeException(interface)
        } else if (!interfaceSet.add(interface.packageDeclaration, interface)) {
          throw new DuplicateImplementedInterfaceException(interface)
        }
    }
  }

  // TODO: If None, it should extend java.lang.Object?
  private def analyzeExtendedClass(superTypeName: Option[NameExpression])(implicit typeDeclaration: TypeDeclaration) {
    superTypeName map {
      superType =>
        if (superType.isInterface) {
          throw new InvalidExtendedTypeException(superType)
        } else if (superType.modifiers contains Modifier.Final) {
          throw new InvalidExtendedClassException(superType)
        }
    }
  }


  private def analyzeClassDeclaration(implicit typeDeclaration: TypeDeclaration) {
    require(!typeDeclaration.isInterface)
    analyzeExtendedClass(typeDeclaration.superType)
    analyzeImplementedInterfaces(typeDeclaration.superInterfaces)
  }

}

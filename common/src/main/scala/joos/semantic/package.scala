package joos

import joos.ast._
import joos.ast.declarations.{PackageDeclaration, TypeDeclaration}
import joos.ast.expressions.NameExpression
import joos.ast.types._
import joos.core.Logger
import scala.Some
import scala.collection.mutable

package object semantic {
  /**
   * You can only call this after the environment is built
   */
  def getTypeDeclaration(name: NameExpression)(implicit unit: CompilationUnit): TypeDeclaration = {
    require(unit != null)
    unit.getVisibleType(name) match {
      case None => {
        val error = s"Cannot resolve ${name} to a type"
        Logger.logError(error)
        // TODO: Better error?
        throw new SemanticException(error)
      }
      case Some(t) => t
    }
  }

  val javaLangObject = NameExpression("java.lang.Object")

  def getSuperType(typeDeclaration: TypeDeclaration): Option[TypeDeclaration] = {
    require(typeDeclaration.compilationUnit != null)
    val compilationUnit = typeDeclaration.compilationUnit
    fullName(typeDeclaration) equals javaLangObject.standardName match {
      case true => None
      case false => {
        Some(
          typeDeclaration.superType match {
            case Some(superType) => getTypeDeclaration(superType)(compilationUnit)
            case None => compilationUnit.javaLangObjectClass
          })
      }
    }
  }

  def getSuperInterfaces(typeDeclaration: TypeDeclaration): Seq[TypeDeclaration] = {
    val compilationUnit = typeDeclaration.compilationUnit
    // TODO: This equals method may not work. We might have to do a fullName comparison.
    if (typeDeclaration equals compilationUnit.javaLangObjectInterface) {
      Seq.empty
    } else if (typeDeclaration.isInterface && typeDeclaration.superInterfaces.isEmpty) {
      Seq(compilationUnit.javaLangObjectInterface)
    } else {
      typeDeclaration.superInterfaces.map(superInterface => getTypeDeclaration(superInterface)(compilationUnit))
    }
  }

  def fullName(typeDeclaration: TypeDeclaration) = {
    require(typeDeclaration.packageDeclaration != null)
    if (typeDeclaration.packageDeclaration == PackageDeclaration.DefaultPackage) {
      typeDeclaration.name.standardName
    } else {
      typeDeclaration.packageDeclaration.name.standardName + '.' + typeDeclaration.name.standardName
    }
  }

  def areEqual(type1: TypeDeclaration, type2: TypeDeclaration): Boolean = {

    require(type1.packageDeclaration != null)
    require(type2.packageDeclaration != null)

    fullName(type1) equals fullName(type2)
  }

  def areEqual(type1: Option[Type], type2: Option[Type])(implicit unit: CompilationUnit): Boolean = {
    (type1, type2) match {
      case (None, None) => true
      case (Some(type1), Some(type2)) => areEqual(type1, type2)
      case _ => false
    }
  }

  def areEqual(type1: Type, type2: Type)(implicit unit: CompilationUnit): Boolean = {
    require(unit != null)
    (type1, type2) match {
      case (SimpleType(name1), SimpleType(name2)) => areEqual(getTypeDeclaration(name1), getTypeDeclaration(name2))
      case (ArrayType(name1, dimensions1), ArrayType(name2, dimensions2)) =>
        dimensions1 == dimensions2 && areEqual(name1, name2)
      case (t1: PrimitiveType, t2: PrimitiveType) => t1 == t2
      case _ => false
    }
  }

  def findDuplicate[T](sequence: Iterable[T]): Option[T] = {
    val set = mutable.HashSet.empty[T]
    sequence foreach {
      element =>
        if (!set.add(element)) {
          return Some(element)
        }
    }
    None
  }

  private def getUpperTypeDeclarations(aType: Type)(implicit unit: CompilationUnit): Set[TypeDeclaration] = {
    aType match {
      case _: PrimitiveType => Set()
      case ArrayType(_, _) => Set()
      case SimpleType(typeName) => {
        unit.getVisibleType(typeName) match {
          case Some(typeDeclaration) => {
            typeDeclaration.allAncestors
          }
          case _ => Set()
        }
      }
    }
  }

  // dst = src
  def isAssignable(dst: Type, src: Type)(implicit unit: CompilationUnit): Boolean = {
    (dst, src) match {
      case (dstPrimitive: PrimitiveType, srcPrimitive: PrimitiveType) => {
        // TODO(Shengmin):
        false
      }
      case (dstArrayType: ArrayType, srcType) => {
        srcType match {
          case srcArrayType: ArrayType => isAssignable(dstArrayType.elementType, srcArrayType.elementType)
          case NullType() => true
          case _ => false
        }
      }
      case (dstSimpleType: SimpleType, srcType) => {
        srcType match {
          case SimpleType(_) => {
            unit.getVisibleType(dstSimpleType.name) match {
              case Some(typeDeclaration) => getUpperTypeDeclarations(srcType).contains(typeDeclaration)
              case None => false
            }
          }
          case NullType() => true
          case _ => false
        }
      }
      case (NullType(), _) => false
      case (unit.javaLangObjectType, ArrayType(_, _)) => true
      case (_, _) => false
    }
  }
}

package joos.semantic

import joos.ast.declarations.{ImportDeclaration, TypeDeclaration}
import joos.ast.expressions.{QualifiedNameExpression, SimpleNameExpression, NameExpression}
import joos.core.Logger
import joos.ast.CompilationUnit
import joos.ast.types.SimpleType

trait CompilationUnitEnvironment extends Environment {
  self: CompilationUnit =>

  val enclosingClass = new NamespaceTrie
  val concreteImports = new NamespaceTrie
  val enclosingPackage = new NamespaceTrie
  val onDemandImports = new NamespaceTrie
  implicit val that = this

  lazy val javaLangObjectClass = getTypeDeclaration(javaLangObject)
  lazy val javaLangObjectInterface = javaLangObjectClass.toInterface
  lazy val javaLangObjectType = SimpleType(javaLangObject)

  lazy val javaLangCloneableInterface = getTypeDeclaration(javaLangCloneable)
  lazy val javaLangCloneableType = SimpleType(javaLangCloneable)

  lazy val javaIOSerializableInterface = getTypeDeclaration(javaLangCloneable)
  lazy val javaIOSerializableType = SimpleType(javaLangCloneable)

  private def addOnDemandImport(name: NameExpression, namespace: NamespaceTrie) {
    moduleDeclaration.namespace.getAllTypesInPackage(name) map {
      typeDeclaration =>
        namespace.add(name, Some(typeDeclaration))
    }
  }

  private def addConcreteImport(packageName: NameExpression, typeDeclaration: TypeDeclaration, namespace: NamespaceTrie) {
    val qualifiedName = QualifiedNameExpression(packageName, typeDeclaration.name)
    concreteImports.getQualifiedType(qualifiedName) match {
      case None => {
        concreteImports.getSimpleType(typeDeclaration.name) match {
          case None => concreteImports.add(packageName, Some(typeDeclaration))
          case _ => throw new NamespaceCollisionException(qualifiedName)
        }
      }
      case _ => Logger.logInformation(s"Attempting to add duplicate concrete import ${typeDeclaration.name}")
    }
  }

  /**
   * Unqualified names are handled by these rules:
   * 1. try the enclosing class or interface
   * 2. try any single-type-import (A.B.C.D)
   * 3. try the same package
   * 4. try any import-on-demand package (A.B.C.*) including java.lang.*
   */
  private def getUnqualifiedType(name: SimpleNameExpression) = {
    var typed = enclosingClass.getSimpleType(name)
    if (typed.isEmpty) {
      typed = concreteImports.getSimpleType(name)
    }
    if (typed.isEmpty) {
      typed = enclosingPackage.getSimpleType(name)
    }
    if (typed.isEmpty) {
      typed = onDemandImports.getSimpleType(name)
    }
    typed
  }

  // No package names or prefixes of package names of declared packages, single-type-import declarations
  // or import-on-demand declarations that are used may resolve to types, except for types in the default package.
  private def getQualifiedType(name: QualifiedNameExpression) = {
    val prefixes = name.standardName.split('.')
    if (prefixes.length >= 2) {
      if (getUnqualifiedType(SimpleNameExpression(prefixes(0))).isDefined) {
        throw new NamespacePrefixException(s"${name} conflicted with ${prefixes(1)} which is a prefix")
      }
      prefixes.dropRight(1).reduceRight {
        (prefix, next) =>
          val qualifiedName = QualifiedNameExpression(NameExpression(prefix), SimpleNameExpression(next))
          if (moduleDeclaration.namespace.getQualifiedType(qualifiedName).isDefined) {
            throw new NamespacePrefixException(s"${name} conflicted with ${qualifiedName.standardName} which is a prefix")
          }
          qualifiedName.standardName
      }
    }
    moduleDeclaration.namespace.getQualifiedType(name)
  }

  /**
   * Gets the type with the {{name}} if it's visible within this compilation unit
   */
  def getVisibleType(name: NameExpression): Option[TypeDeclaration] = {
    name match {
      case simpleName: SimpleNameExpression => getUnqualifiedType(simpleName)
      case qualifiedName: QualifiedNameExpression => getQualifiedType(qualifiedName)
    }
  }

  def add(importDeclaration: ImportDeclaration): this.type = {
    if (importDeclaration.isOnDemand) {
      addOnDemandImport(importDeclaration.name, onDemandImports)
    } else {
      importDeclaration.name match {
        case e@QualifiedNameExpression(qualifier, typeName) => {
          moduleDeclaration.namespace.getQualifiedType(e) match {
            case Some(typeDeclaration) => addConcreteImport(qualifier, typeDeclaration, concreteImports)
            case _ => throw new MissingTypeException(importDeclaration.name)
          }
        }
        case _ => {
          Logger.logError(
            s"ImportDeclaration was given a SimpleNameExpression ${
              importDeclaration.name
            }")
          throw new RuntimeException("This also shouldnt happen")
        }
      }
    }
    this
  }

  def addSelfPackage() {
    typeDeclaration map (addConcreteImport(packageDeclaration.name, _, enclosingClass))
    addOnDemandImport(packageDeclaration.name, enclosingPackage)
    addOnDemandImport(NameExpression("java.lang"), onDemandImports)
    importDeclarations foreach add
  }

}

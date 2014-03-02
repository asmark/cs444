package joos.semantic

import joos.ast.CompilationUnit
import joos.ast.declarations.{PackageDeclaration, ImportDeclaration, TypeDeclaration}
import joos.ast.expressions.{QualifiedNameExpression, SimpleNameExpression, NameExpression}
import scala.collection.mutable

trait CompilationUnitEnvironment extends Environment {
  self: CompilationUnit =>

  val concreteImports = new NamespaceTrie
  val onDemandImports = mutable.Set.empty[NameExpression]

  private def getTypeFromOnDemandImports(name: SimpleNameExpression) = {
    onDemandImports.map(t => (t -> moduleDeclaration.namespace.getQualifiedType(QualifiedNameExpression(t, name)))).foreach {
      qualifiedName: (NameExpression, Option[TypeDeclaration]) =>
        val packageName = qualifiedName._1
        val typeName = qualifiedName._2
        typeName map (t => addConcreteImport(packageName, t))
    }
  }

  private def getTypeFromConcreteImports(name: SimpleNameExpression) = concreteImports.getSimpleType(name)

  private def addConcreteImport(packageName: NameExpression, typeDeclaration: TypeDeclaration) {
    if (concreteImports.getSimpleType(typeDeclaration.name).isDefined) {
      throw new NamespaceCollisionException(typeDeclaration.name)
    } else {
      concreteImports.add(packageName, Some(typeDeclaration))
    }
  }

  private def addConcreteImport(packageName: NameExpression, typeDeclaration: Option[TypeDeclaration]) {
    concreteImports.add(packageName, typeDeclaration)
  }

  /**
   * Gets the type with the {{name}} if it's visible within this compilation unit
   */
  def getVisibleType(name: NameExpression): Option[TypeDeclaration] = {
    name match {
      case simpleName: SimpleNameExpression => {
        getTypeFromOnDemandImports(simpleName)
        getTypeFromConcreteImports(simpleName)
      }
      case qualifiedName: QualifiedNameExpression => moduleDeclaration.namespace.getQualifiedType(qualifiedName)
    }
  }

  // TODO: Check if overwriting imports?
  def add(importDeclaration: ImportDeclaration): this.type = {
    if (importDeclaration.isOnDemand) {
      onDemandImports.add(importDeclaration.name)
    } else {
      importDeclaration.name match {
        case e@QualifiedNameExpression(qualifier, typeName) => {
          moduleDeclaration.namespace.getQualifiedType(e) match {
            case Some(typeDeclaration) => addConcreteImport(qualifier, typeDeclaration)
            case _ => throw new MissingTypeException(importDeclaration.name)
          }
        }
        case _ => throw new RuntimeException("This also shouldnt happen")
      }
    }
    this
  }

  // TODO: Check if overwriting classes?
  def addDefaultPackage(): this.type = {
    val defaultPackageName = PackageDeclaration.DefaultPackage.name
    moduleDeclaration.namespace.getAllTypesInPackage(defaultPackageName) foreach {
      typeDeclaration =>
          addConcreteImport(defaultPackageName, typeDeclaration)
    }
    this
  }

  def addSelfPackage(): this.type = {
    addConcreteImport(packageDeclaration.name, typeDeclaration)
    this
  }

}

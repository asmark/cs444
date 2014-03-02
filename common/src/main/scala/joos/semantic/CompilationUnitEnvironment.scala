package joos.semantic

import joos.ast.CompilationUnit
import joos.ast.declarations.{ImportDeclaration, TypeDeclaration}
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
        typeName map (t => concreteImports.add(packageName, Some(t)))
    }
  }

  private def getTypeFromConcreteImports(name: SimpleNameExpression) = concreteImports.getSimpleType(name)

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
            case Some(typeDeclaration) => concreteImports.add(qualifier, Some(typeDeclaration))
            case _ => throw new InvalidImportException(importDeclaration.name)
          }
        }
        case _ => throw new RuntimeException("This also shouldnt happen")
      }
    }
    this
  }

  // TODO: Check if overwriting classes?
  def addDefaultPackage(): this.type = {
    val defaultPackageName = NameExpression("")
    moduleDeclaration.namespace.getAllTypesInPackage(defaultPackageName) foreach {
      typeDeclaration =>
        concreteImports.add(defaultPackageName, Some(typeDeclaration))
    }
    this
  }

  def addSelfPackage(): this.type = {
    concreteImports.add(packageDeclaration.name, typeDeclaration)
    this
  }

}

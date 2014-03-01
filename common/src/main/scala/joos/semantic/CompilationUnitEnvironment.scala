package joos.semantic

import joos.ast.CompilationUnit
import joos.ast.declarations.{PackageDeclaration, ImportDeclaration, TypeDeclaration}
import joos.ast.expressions.{QualifiedNameExpression, SimpleNameExpression, NameExpression}
import scala.collection.mutable

trait CompilationUnitEnvironment extends Environment {
  self: CompilationUnit =>

  val onDemandImports = mutable.HashMap.empty[NameExpression, PackageDeclaration]
  val concreteImports = mutable.HashMap.empty[NameExpression, TypeDeclaration]

  private def getTypeFromConcreteImports(name: SimpleNameExpression) = {
    Seq(concreteImports.get(name))
  }

  private def getTypeFromOnDemandImports(name: SimpleNameExpression) = {
    onDemandImports.values.foldRight(Seq.empty[Option[TypeDeclaration]]) {
      (declaration, list) => declaration.getType(name) +: list
    }
  }

  // TODO: Visibility for types is always public?
  private def checkVisibilityAndDuplicates(typeDeclarations: Seq[Option[TypeDeclaration]]) = {
    val matchingDeclarations = typeDeclarations flatMap identity[Option[TypeDeclaration]] // Get rid of options
    if (matchingDeclarations.size > 1) throw new DuplicateImportException(matchingDeclarations.head.name) else matchingDeclarations.headOption
  }

  /**
   * Gets the type with the {{name}} if it's visible within this compilation unit
   */
  def getVisibleType(name: NameExpression): Option[TypeDeclaration] = {
    name match {
      case simpleName: SimpleNameExpression => checkVisibilityAndDuplicates(
        getTypeFromConcreteImports(simpleName) ++ getTypeFromOnDemandImports(
          simpleName))
      case qualifiedName: QualifiedNameExpression => checkVisibilityAndDuplicates(Seq(moduleDeclaration.getType(name)))
    }
  }

  // TODO: Check if overwriting imports?
  def add(importDeclaration: ImportDeclaration): this.type = {
    if (importDeclaration.isOnDemand) {
      val packageName = importDeclaration.name
      moduleDeclaration.getPackage(packageName) match {
        case None => throw new InvalidImportException(packageName)
        case Some(`packageDeclaration`) => throw new InvalidImportException(packageName) // Importing self package
        case Some(packageDeclaration) => onDemandImports.put(packageName, packageDeclaration)
      }
    } else {
      val typeName = importDeclaration.name
      moduleDeclaration.getType(typeName) match {
        case None => throw new InvalidImportException(typeName)
        case Some(`typeDeclaration`) => throw new InvalidImportException(typeName) // Importing self type
        case Some(typeDeclaration) => {
          concreteImports.put(typeName, typeDeclaration)
        }
      }
    }
    this
  }

  // TODO: Check if overwriting classes?
  def addDefaultPackage(): this.type = {
    PackageDeclaration.DefaultPackage.getTypeDeclarations.foreach(declaration => concreteImports.put(declaration.name, declaration))
    this
  }

  def addSelfPackage(): this.type = {
    packageDeclaration.getTypeDeclarations.foreach(declaration => concreteImports.put(declaration.name, declaration))
    this
  }

}

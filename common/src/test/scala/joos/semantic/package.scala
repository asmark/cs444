package joos

import joos.ast.declarations.{ImportDeclaration, TypeDeclaration, PackageDeclaration, ModuleDeclaration}
import joos.ast.expressions.{NameExpression, SimpleNameExpression}
import joos.ast.{Modifier, CompilationUnit}

package object semantic {
  def MockPackage1 = PackageDeclaration("mock.pkg")
  def MockPackage2 = PackageDeclaration("mock")
  def MockDefaultPackage = PackageDeclaration.DefaultPackage

  def MockTypeName1 = "MockType1"
  def MockTypeName2 = "MockType2"
  def MockDefaultTypeName1 = "MockDefault1"
  def MockDefaultTypeName2 = "MockDefault2"

  def MockTypeDeclaration1 = TypeDeclaration(Seq(Modifier.Public), false, SimpleNameExpression(MockTypeName1), None, Seq.empty, Seq.empty, Seq.empty)
  def MockTypeDeclaration2 = TypeDeclaration(Seq(Modifier.Public), false, SimpleNameExpression(MockTypeName2), None, Seq.empty, Seq.empty, Seq.empty)
  def MockDefaultDeclaration1 = TypeDeclaration(
    Seq(Modifier.Public), false, SimpleNameExpression(MockDefaultTypeName1),
    None, Seq.empty, Seq.empty, Seq.empty)
  def MockDefaultDeclaration2 = TypeDeclaration(
    Seq(Modifier.Public), false, SimpleNameExpression(MockDefaultTypeName2),
    None, Seq.empty, Seq.empty, Seq.empty)

  def mockImport(packageDeclaration: PackageDeclaration, name: Option[SimpleNameExpression]) = {
    if (name.isDefined) {
      val newName = NameExpression(packageDeclaration.name.standardName + "." + name.get.standardName)
      ImportDeclaration(newName, false)
    } else {
      ImportDeclaration(packageDeclaration.name, true)
    }
  }

  def mockModule(compilationUnits: Seq[CompilationUnit]) = {
    val mock = new ModuleDeclaration
    compilationUnits foreach {
      unit =>
        mock.add(unit)
        unit.typeDeclaration map (unit.packageDeclaration.add(_))
    }
    compilationUnits foreach {
      unit =>
        unit.importDeclarations foreach (unit.add(_))
        unit.addDefaultPackage()
        unit.addSelfPackage()
    }

    mock
  }
}

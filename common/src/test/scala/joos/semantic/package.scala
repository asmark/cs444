package joos

import joos.ast.declarations.{ImportDeclaration, TypeDeclaration, PackageDeclaration, ModuleDeclaration}
import joos.ast.expressions.{QualifiedNameExpression, NameExpression, SimpleNameExpression}
import joos.ast.{Modifier, CompilationUnit}

package object semantic {
  def MockPackage1 = PackageDeclaration("mock.pkg")
  def MockPackage2 = PackageDeclaration("mock")
  def MockDefaultPackage = PackageDeclaration.DefaultPackage

  def MockSimpleTypeName1 = SimpleNameExpression("MockType1")
  def MockSimpleTypeName2 = SimpleNameExpression("MockType2")
  def MockSimpleDefaultTypeName1 = SimpleNameExpression("MockDefault1")
  def MockSimpleDefaultTypeName2 = SimpleNameExpression("MockDefault2")

  def MockQualifiedTypeName1 = QualifiedNameExpression(MockPackage1.name, MockSimpleTypeName1)
  def MockQualifiedTypeName2 = QualifiedNameExpression(MockPackage2.name, MockSimpleTypeName2)

  def MockTypeDeclaration1 = TypeDeclaration(Seq(Modifier.Public), false, MockSimpleTypeName1, None, Seq.empty, Seq.empty, Seq.empty)
  def MockTypeDeclaration2 = TypeDeclaration(Seq(Modifier.Public), false, MockSimpleTypeName2, None, Seq.empty, Seq.empty, Seq.empty)
  def MockDefaultDeclaration1 = TypeDeclaration(
    Seq(Modifier.Public), false, MockSimpleDefaultTypeName1,
    None, Seq.empty, Seq.empty, Seq.empty)
  def MockDefaultDeclaration2 = TypeDeclaration(
    Seq(Modifier.Public), false, MockSimpleDefaultTypeName2,
    None, Seq.empty, Seq.empty, Seq.empty)

  def mockImport(packageDeclaration: PackageDeclaration, name: Option[SimpleNameExpression]) = {
    if (name.isDefined) {
      val newName = NameExpression(packageDeclaration.name.standardName + "." + name.get.standardName)
      ImportDeclaration(newName, false)
    } else {
      ImportDeclaration(packageDeclaration.name, true)
    }
  }

  // Links the compilation units
  def mockModule(compilationUnits: Seq[CompilationUnit]) = {
    val mock = new ModuleDeclaration
    compilationUnits foreach {
      unit =>
        mock.add(unit)
        unit.moduleDeclaration = mock
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

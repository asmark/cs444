package joos

import joos.ast.declarations.{ImportDeclaration, TypeDeclaration, PackageDeclaration, ModuleDeclaration}
import joos.ast.expressions.{NameExpression, SimpleNameExpression}
import joos.ast.{Modifier, CompilationUnit}

package object semantic {
  val MockPackage1 = PackageDeclaration("mock.pkg")
  val MockPackage2 = PackageDeclaration("mock")
  val MockDefaultPackage = PackageDeclaration.DefaultPackage

  val MockTypeName1 = "MockType1"
  val MockTypeName2 = "MockType2"
  val MockDefaultTypeName1 = "MockDefault1"
  val MockDefaultTypeName2 = "MockDefault2"

  val MockTypeDeclaration1 = TypeDeclaration(Seq(Modifier.Public), false, SimpleNameExpression(MockTypeName1), None, Seq.empty, Seq.empty, Seq.empty)
  val MockTypeDeclaration2 = TypeDeclaration(Seq(Modifier.Public), false, SimpleNameExpression(MockTypeName2), None, Seq.empty, Seq.empty, Seq.empty)
  val MockDefaultDeclaration1 = TypeDeclaration(
    Seq(Modifier.Public), false, SimpleNameExpression(MockDefaultTypeName1),
    None, Seq.empty, Seq.empty, Seq.empty)
  val MockDefaultDeclaration2 = TypeDeclaration(
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

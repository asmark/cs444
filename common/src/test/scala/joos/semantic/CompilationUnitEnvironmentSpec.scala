package joos.semantic

import org.scalatest.{BeforeAndAfterEach, Matchers, FlatSpec}
import joos.ast.CompilationUnit
import joos.ast.expressions.NameExpression
import joos.ast.declarations.PackageDeclaration

class CompilationUnitEnvironmentSpec extends FlatSpec with Matchers with BeforeAndAfterEach {

  override def beforeEach() = PackageDeclaration.DefaultPackage.clearEnvironment()

  "Zero imports and no type declaration" should "only resolve types within default package" in {
    val defaultUnit1 = CompilationUnit(MockDefaultPackage, Seq.empty, Some(MockDefaultDeclaration1))
    val defaultUnit2 = CompilationUnit(MockDefaultPackage, Seq.empty, Some(MockDefaultDeclaration2))
    val unit = CompilationUnit(MockPackage1, Seq.empty, None)
    mockModule(Seq(unit, defaultUnit1, defaultUnit2))

    unit.getVisibleType(NameExpression(MockDefaultTypeName1)) shouldBe Some(MockDefaultDeclaration1)
    unit.getVisibleType(NameExpression(MockDefaultTypeName2)) shouldBe Some(MockDefaultDeclaration2)
    unit.getVisibleType(NameExpression(MockTypeName1)) shouldBe None
  }

  "Zero imports with a type declaration" should "only resolve types within default package and self" in {
    val defaultUnit1 = CompilationUnit(MockDefaultPackage, Seq.empty, Some(MockDefaultDeclaration1))
    val defaultUnit2 = CompilationUnit(MockDefaultPackage, Seq.empty, Some(MockDefaultDeclaration2))
    val unit = CompilationUnit(MockPackage1, Seq.empty, Some(MockTypeDeclaration1))
    mockModule(Seq(unit, defaultUnit1, defaultUnit2))

    unit.getVisibleType(NameExpression(MockDefaultTypeName1)) shouldBe Some(MockDefaultDeclaration1)
    unit.getVisibleType(NameExpression(MockDefaultTypeName2)) shouldBe Some(MockDefaultDeclaration2)
    unit.getVisibleType(NameExpression(MockTypeName1)) shouldBe Some(MockTypeDeclaration1)
  }

  "Zero imports" should "not resolve outside packages" in {
    val unit1 = CompilationUnit(MockPackage1, Seq.empty, Some(MockTypeDeclaration1))
    val unit2 = CompilationUnit(MockPackage2, Seq.empty, Some(MockTypeDeclaration1))

  }


  // Test Compilation Units
  // ... with no imports (Default package)
  // ... with no imports (Default package and self)
  // ... with one concrete import
  // ... with one on demand import with no ambiguities
  // ... with one on demand import with ambiguities
  // ... with multiple concrete imports with ambiguities
  // ... with one concrete import and on demand with ambiguity


}

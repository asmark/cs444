package joos.semantic

import org.scalatest.{BeforeAndAfterEach, Matchers, FlatSpec}
import joos.ast.CompilationUnit
import joos.ast.expressions.QualifiedNameExpression
import joos.ast.declarations.PackageDeclaration

class CompilationUnitEnvironmentSpec extends FlatSpec with Matchers with BeforeAndAfterEach {

  override def beforeEach() = PackageDeclaration.DefaultPackage.clearEnvironment()

  "Zero imports and no type declaration" should "only resolve SimpleNames within default package" in {
    val defaultUnit1 = CompilationUnit(MockDefaultPackage, Seq.empty, Some(MockDefaultDeclaration1))
    val defaultUnit2 = CompilationUnit(MockDefaultPackage, Seq.empty, Some(MockDefaultDeclaration2))
    val unit = CompilationUnit(MockPackage1, Seq.empty, None)
    mockModule(Seq(unit, defaultUnit1, defaultUnit2))

    unit.getVisibleType(MockSimpleDefaultTypeName1) shouldBe Some(MockDefaultDeclaration1)
    unit.getVisibleType(MockSimpleDefaultTypeName2) shouldBe Some(MockDefaultDeclaration2)
    unit.getVisibleType(MockSimpleTypeName1) shouldBe None
  }

  "Zero imports with a type declaration" should "only resolve SimpleNames within default package and self" in {
    val defaultUnit1 = CompilationUnit(MockDefaultPackage, Seq.empty, Some(MockDefaultDeclaration1))
    val defaultUnit2 = CompilationUnit(MockDefaultPackage, Seq.empty, Some(MockDefaultDeclaration2))
    val unit = CompilationUnit(MockPackage1, Seq.empty, Some(MockTypeDeclaration1))
    mockModule(Seq(unit, defaultUnit1, defaultUnit2))

    unit.getVisibleType(MockSimpleDefaultTypeName1) shouldBe Some(MockDefaultDeclaration1)
    unit.getVisibleType(MockSimpleDefaultTypeName2) shouldBe Some(MockDefaultDeclaration2)
    unit.getVisibleType(MockSimpleTypeName1) shouldBe Some(MockTypeDeclaration1)
  }

  "Zero imports" should "not resolve SimpleNames of outside packages" in {
    val unit1 = CompilationUnit(MockPackage1, Seq.empty, Some(MockTypeDeclaration1))
    val unit2 = CompilationUnit(MockPackage2, Seq.empty, Some(MockTypeDeclaration2))
    val defaultUnit = CompilationUnit(MockDefaultPackage, Seq.empty, Some(MockDefaultDeclaration1))

    mockModule(Seq(unit1, unit2, defaultUnit))

    unit1.getVisibleType(MockSimpleDefaultTypeName1) shouldBe Some(MockDefaultDeclaration1)
    unit1.getVisibleType(MockSimpleTypeName1) shouldBe Some(MockTypeDeclaration1)
    unit1.getVisibleType(MockSimpleTypeName2) shouldBe None

    unit2.getVisibleType(MockSimpleDefaultTypeName1) shouldBe Some(MockDefaultDeclaration1)
    unit2.getVisibleType(MockSimpleTypeName1) shouldBe None
    unit2.getVisibleType(MockSimpleTypeName2) shouldBe Some(MockTypeDeclaration2)
  }

  "Zero imports" should "resolve QualifiedNames of outside packages" in {
    val unit1 = CompilationUnit(MockPackage1, Seq.empty, Some(MockTypeDeclaration1))
    val unit2 = CompilationUnit(MockPackage2, Seq.empty, Some(MockTypeDeclaration2))
    val defaultUnit = CompilationUnit(MockDefaultPackage, Seq.empty, Some(MockDefaultDeclaration1))

    mockModule(Seq(unit1, unit2, defaultUnit))

    unit1.getVisibleType(MockSimpleDefaultTypeName1) shouldBe Some(MockDefaultDeclaration1)
    unit1.getVisibleType(MockSimpleTypeName1) shouldBe Some(MockTypeDeclaration1)
    unit1.getVisibleType(MockQualifiedTypeName2) shouldBe Some(MockTypeDeclaration2)

    unit2.getVisibleType(MockSimpleDefaultTypeName1) shouldBe Some(MockDefaultDeclaration1)
    unit2.getVisibleType(MockQualifiedTypeName1) shouldBe Some(MockTypeDeclaration1)
    unit2.getVisibleType(MockSimpleTypeName2) shouldBe Some(MockTypeDeclaration2)
  }

  "A concrete import" should "resolve as a SimpleName" in {
    val unit1 = CompilationUnit(MockPackage1, Seq(mockImport(MockPackage2, None)), Some(MockTypeDeclaration1))
    val unit2 = CompilationUnit(MockPackage2, Seq.empty, Some(MockTypeDeclaration2))

    mockModule(Seq(unit1, unit2))

    unit1.getVisibleType(MockSimpleTypeName1) shouldBe Some(MockTypeDeclaration1)
    unit1.getVisibleType(MockSimpleTypeName2) shouldBe Some(MockTypeDeclaration2)

    unit2.getVisibleType(MockSimpleTypeName2) shouldBe Some(MockTypeDeclaration2)
    unit2.getVisibleType(MockSimpleTypeName1) shouldBe None
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

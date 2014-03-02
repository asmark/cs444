package joos.semantic

import joos.ast.CompilationUnit
import org.scalatest.{BeforeAndAfterEach, Matchers, FlatSpec}

class CompilationUnitEnvironmentSpec extends FlatSpec with Matchers with BeforeAndAfterEach {

  "Zero imports and no package declaration" should "only resolve SimpleNames within default package" in {
    val defaultUnit1 = CompilationUnit(MockDefaultPackage, Seq.empty, Some(MockDefaultDeclaration1))
    val defaultUnit2 = CompilationUnit(MockDefaultPackage, Seq.empty, Some(MockDefaultDeclaration2))
    val unit = CompilationUnit(MockDefaultPackage, Seq.empty, None)
    mockLink(Seq(unit, defaultUnit1, defaultUnit2))

    unit.getVisibleType(MockSimpleDefaultTypeName1) shouldBe Some(MockDefaultDeclaration1)
    unit.getVisibleType(MockSimpleDefaultTypeName2) shouldBe Some(MockDefaultDeclaration2)
    unit.getVisibleType(MockSimpleTypeName1) shouldBe None
  }

  "Zero imports with a package declaration" should "only resolve SimpleNames in self package" in {
    val defaultUnit1 = CompilationUnit(MockDefaultPackage, Seq.empty, Some(MockDefaultDeclaration1))
    val defaultUnit2 = CompilationUnit(MockDefaultPackage, Seq.empty, Some(MockDefaultDeclaration2))
    val unit = CompilationUnit(MockPackage1, Seq.empty, Some(MockTypeDeclaration1))
    mockLink(Seq(unit, defaultUnit1, defaultUnit2))

    unit.getVisibleType(MockSimpleDefaultTypeName1) shouldBe None
    unit.getVisibleType(MockSimpleDefaultTypeName2) shouldBe None
    unit.getVisibleType(MockSimpleTypeName1) shouldBe Some(MockTypeDeclaration1)
  }

  "Zero imports" should "not resolve SimpleNames of outside packages" in {
    val unit1 = CompilationUnit(MockPackage1, Seq.empty, Some(MockTypeDeclaration1))
    val unit2 = CompilationUnit(MockPackage2, Seq.empty, Some(MockTypeDeclaration2))
    val defaultUnit = CompilationUnit(MockDefaultPackage, Seq.empty, Some(MockDefaultDeclaration1))

    mockLink(Seq(unit1, unit2, defaultUnit))

    unit1.getVisibleType(MockSimpleDefaultTypeName1) shouldBe None
    unit1.getVisibleType(MockSimpleTypeName1) shouldBe Some(MockTypeDeclaration1)
    unit1.getVisibleType(MockSimpleTypeName2) shouldBe None

    unit2.getVisibleType(MockSimpleDefaultTypeName1) shouldBe None
    unit2.getVisibleType(MockSimpleTypeName1) shouldBe None
    unit2.getVisibleType(MockSimpleTypeName2) shouldBe Some(MockTypeDeclaration2)

    defaultUnit.getVisibleType(MockSimpleDefaultTypeName1) shouldBe Some(MockDefaultDeclaration1)
    defaultUnit.getVisibleType(MockSimpleTypeName1) shouldBe None
    defaultUnit.getVisibleType(MockSimpleTypeName2) shouldBe None
  }

  "Zero imports" should "resolve QualifiedNames of outside packages" in {
    val unit1 = CompilationUnit(MockPackage1, Seq.empty, Some(MockTypeDeclaration1))
    val unit2 = CompilationUnit(MockPackage2, Seq.empty, Some(MockTypeDeclaration2))
    val defaultUnit = CompilationUnit(MockDefaultPackage, Seq.empty, Some(MockDefaultDeclaration1))

    mockLink(Seq(unit1, unit2, defaultUnit))

    unit1.getVisibleType(MockSimpleDefaultTypeName1) shouldBe None
    unit1.getVisibleType(MockSimpleTypeName1) shouldBe Some(MockTypeDeclaration1)
    unit1.getVisibleType(MockQualifiedTypeName2) shouldBe Some(MockTypeDeclaration2)

    unit2.getVisibleType(MockSimpleDefaultTypeName1) shouldBe None
    unit2.getVisibleType(MockQualifiedTypeName1) shouldBe Some(MockTypeDeclaration1)
    unit2.getVisibleType(MockSimpleTypeName2) shouldBe Some(MockTypeDeclaration2)

    defaultUnit.getVisibleType(MockSimpleDefaultTypeName1) shouldBe Some(MockDefaultDeclaration1)
    defaultUnit.getVisibleType(MockQualifiedTypeName1) shouldBe Some(MockTypeDeclaration1)
    defaultUnit.getVisibleType(MockQualifiedTypeName2) shouldBe Some(MockTypeDeclaration2)
  }

  "An on-demand import" should "resolve as a SimpleName" in {
    val unit1 = CompilationUnit(MockPackage1, Seq(mockImport(MockPackage2, None)), Some(MockTypeDeclaration1))
    val unit2 = CompilationUnit(MockPackage2, Seq.empty, Some(MockTypeDeclaration2))

    mockLink(Seq(unit1, unit2))

    unit1.getVisibleType(MockSimpleTypeName1) shouldBe Some(MockTypeDeclaration1)
    unit1.getVisibleType(MockSimpleTypeName2) shouldBe Some(MockTypeDeclaration2)

    unit2.getVisibleType(MockSimpleTypeName2) shouldBe Some(MockTypeDeclaration2)
    unit2.getVisibleType(MockSimpleTypeName1) shouldBe None
  }

  "An concrete import" should "resolve as a SimpleName" in {
    val unit1 = CompilationUnit(MockPackage1, Seq(mockImport(MockPackage2, Some(MockSimpleTypeName2))), Some(MockTypeDeclaration1))
    val unit2 = CompilationUnit(MockPackage2, Seq.empty, Some(MockTypeDeclaration2))

    mockLink(Seq(unit1, unit2))

    unit1.getVisibleType(MockSimpleTypeName1) shouldBe Some(MockTypeDeclaration1)
    unit1.getVisibleType(MockSimpleTypeName2) shouldBe Some(MockTypeDeclaration2)

    unit2.getVisibleType(MockSimpleTypeName2) shouldBe Some(MockTypeDeclaration2)
    unit2.getVisibleType(MockSimpleTypeName1) shouldBe None
  }

  "A concrete and on-demand import that overlap" should "resolve as a SimpleName" in {
    val unit1 = CompilationUnit(
      MockPackage1,
      Seq(mockImport(MockPackage2, None), mockImport(MockPackage2, Some(MockSimpleTypeName2))),
      Some(MockTypeDeclaration1))
    val unit2 = CompilationUnit(MockPackage2, Seq.empty, Some(MockTypeDeclaration2))

    mockLink(Seq(unit1, unit2))

    unit1.getVisibleType(MockSimpleTypeName1) shouldBe Some(MockTypeDeclaration1)
    unit1.getVisibleType(MockSimpleTypeName2) shouldBe Some(MockTypeDeclaration2)

    unit2.getVisibleType(MockSimpleTypeName2) shouldBe Some(MockTypeDeclaration2)
    unit2.getVisibleType(MockSimpleTypeName1) shouldBe None
  }

  "Two identical concrete imports" should "resolve as a SimpleName" in {
    val unit1 = CompilationUnit(
      MockPackage1,
      Seq(mockImport(MockPackage2, Some(MockSimpleTypeName2)), mockImport(MockPackage2, Some(MockSimpleTypeName2))),
      Some(MockTypeDeclaration1))
    val unit2 = CompilationUnit(MockPackage2, Seq.empty, Some(MockTypeDeclaration2))

    mockLink(Seq(unit1, unit2))

    unit1.getVisibleType(MockSimpleTypeName1) shouldBe Some(MockTypeDeclaration1)
    unit1.getVisibleType(MockSimpleTypeName2) shouldBe Some(MockTypeDeclaration2)

    unit2.getVisibleType(MockSimpleTypeName2) shouldBe Some(MockTypeDeclaration2)
    unit2.getVisibleType(MockSimpleTypeName1) shouldBe None
  }

  "A concrete import that collides with a current type" should "throw an exception" in {
    val unit1 = CompilationUnit(
      MockPackage1,
      Seq(mockImport(MockPackage2, Some(MockSimpleTypeName1))),
      Some(MockTypeDeclaration1))
    val unit2 = CompilationUnit(MockPackage2, Seq.empty, Some(MockTypeDeclaration1))

    intercept[NamespaceCollisionException] {
      mockLink(Seq(unit1, unit2))
    }
  }

  "Two colliding concrete imports" should "throw an exception" in {
    val unit1 = CompilationUnit(
      MockDefaultPackage,
      Seq(mockImport(MockPackage1, Some(MockSimpleTypeName1)), mockImport(MockPackage2, Some(MockSimpleTypeName1))),
      None)
    val unit2 = CompilationUnit(MockPackage1, Seq.empty, Some(MockTypeDeclaration1))
    val unit3 = CompilationUnit(MockPackage2, Seq.empty, Some(MockTypeDeclaration1))

    intercept[NamespaceCollisionException] {
      mockLink(Seq(unit1, unit2, unit3))
    }
  }

  "A non-existing concrete import" should "throw an exception" in {
    val unit1 = CompilationUnit(
      MockDefaultPackage,
      Seq(mockImport(MockPackage1, Some(MockSimpleTypeName1))),
      None)

    intercept[MissingTypeException] {
      mockLink(Seq(unit1))
    }
  }

  "A non-existing on-demand import" should "throw an exception" in {
    val unit1 = CompilationUnit(
      MockDefaultPackage,
      Seq(mockImport(MockPackage1, None)),
      None)

    intercept[MissingTypeException] {
      mockLink(Seq(unit1))
    }
  }

  "Two colliding on-demand imports" should "throw an exception" in {
    val unit1 = CompilationUnit(
      MockDefaultPackage,
      Seq(mockImport(MockPackage1, None), mockImport(MockPackage2, None)),
      None)
    val unit2 = CompilationUnit(MockPackage1, Seq.empty, Some(MockTypeDeclaration1))
    val unit3 = CompilationUnit(MockPackage2, Seq.empty, Some(MockTypeDeclaration1))

    mockLink(Seq(unit1, unit2, unit3))

    intercept[NamespaceCollisionException] {
      unit1.getVisibleType(MockSimpleTypeName1)
    }
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

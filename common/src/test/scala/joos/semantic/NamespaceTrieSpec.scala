package joos.semantic

import joos.ast.expressions.{NameExpression, QualifiedNameExpression}
import org.scalatest.{Matchers, FlatSpec}

class NamespaceTrieSpec extends FlatSpec with Matchers {

  "Types in the default package" should "be visible" in {
    val namespace = new NamespaceTrie
    namespace.add(MockDefaultPackage.name, Some(MockTypeDeclaration1))

    namespace.getAllTypesInPackage(MockDefaultPackage.name) shouldEqual Seq(MockTypeDeclaration1)
    namespace.getSimpleType(MockSimpleTypeName1) shouldBe Some(MockTypeDeclaration1)
    namespace.getQualifiedType(QualifiedNameExpression(MockDefaultPackage.name, MockSimpleTypeName1)) shouldBe Some(MockTypeDeclaration1)
  }

  "Types in an added package" should "be visible" in {
    val namespace = new NamespaceTrie
    namespace.add(MockPackage1.name, Some(MockTypeDeclaration1))
    namespace.add(MockPackage1.name, Some(MockTypeDeclaration2))

    namespace.getAllTypesInPackage(MockPackage1.name) should contain theSameElementsAs Seq(MockTypeDeclaration1, MockTypeDeclaration2)
    namespace.getSimpleType(MockSimpleTypeName1) shouldBe Some(MockTypeDeclaration1)
    namespace.getSimpleType(MockSimpleTypeName2) shouldBe Some(MockTypeDeclaration2)
    namespace.getQualifiedType(MockQualifiedTypeName1) shouldBe Some(MockTypeDeclaration1)
    namespace.getQualifiedType(QualifiedNameExpression(MockPackage1.name, MockSimpleTypeName2)) shouldBe Some(MockTypeDeclaration2)
  }

  "Multiple types in different packages" should "be visible" in {
    val namespace = new NamespaceTrie
    namespace.add(MockPackage1.name, Some(MockTypeDeclaration1))
    namespace.add(MockPackage2.name, Some(MockTypeDeclaration2))

    namespace.getAllTypesInPackage(MockPackage1.name) should contain theSameElementsAs Seq(MockTypeDeclaration1)
    namespace.getAllTypesInPackage(MockPackage2.name) should contain theSameElementsAs Seq(MockTypeDeclaration2)
    namespace.getSimpleType(MockSimpleTypeName1) shouldBe Some(MockTypeDeclaration1)
    namespace.getSimpleType(MockSimpleTypeName2) shouldBe Some(MockTypeDeclaration2)
    namespace.getQualifiedType(MockQualifiedTypeName1) shouldBe Some(MockTypeDeclaration1)
    namespace.getQualifiedType(MockQualifiedTypeName2) shouldBe Some(MockTypeDeclaration2)
  }

  "Equivalent names in different packages" should "be visible" in {
    val namespace = new NamespaceTrie
    namespace.add(MockPackage1.name, Some(MockTypeDeclaration1))
    namespace.add(MockPackage2.name, Some(MockTypeDeclaration1))

    namespace.getQualifiedType(QualifiedNameExpression(MockPackage1.name, MockSimpleTypeName1)) shouldBe Some(MockTypeDeclaration1)
    namespace.getQualifiedType(QualifiedNameExpression(MockPackage2.name, MockSimpleTypeName1)) shouldBe Some(MockTypeDeclaration1)
  }

  "Multiple SimpleNames in a namespace" should "throw a collision exception" in {
    val namespace = new NamespaceTrie
    namespace.add(MockPackage1.name, Some(MockTypeDeclaration1))
    namespace.add(MockPackage2.name, Some(MockTypeDeclaration1))

    intercept[NamespaceCollisionException] {
      namespace.getSimpleType(MockSimpleTypeName1)
    }
  }

  "Adding a class that is a prefix of an existing package" should "throw a collision exception" in {
    val namespace = new NamespaceTrie
    namespace.add(NameExpression(s"foo.${MockSimpleTypeName1}"), Some(MockTypeDeclaration2))
    intercept[NamespaceCollisionException] {
      namespace.add(NameExpression("foo" ), Some(MockTypeDeclaration1))
    }
  }

  "Adding a package that is a suffix of an existing class" should "throw a collision exception" in {
    val namespace = new NamespaceTrie
    namespace.add(NameExpression("foo"), Some(MockTypeDeclaration1))
    intercept[NamespaceCollisionException] {
      namespace.add(NameExpression(s"foo.${MockSimpleTypeName1}"), Some(MockTypeDeclaration2))
    }
  }

  "Referencing types that aren't in the namespace" should "return None" in {
  val namespace = new NamespaceTrie
  namespace.add(MockPackage2.name, Some(MockTypeDeclaration2))

    intercept[MissingTypeException] {
      namespace.getAllTypesInPackage(MockPackage1.name)
    }

    intercept[MissingTypeException] {
      namespace.getAllTypesInPackage(NameExpression("foo.bar"))
    }

    namespace.getSimpleType(MockSimpleTypeName1) shouldBe None
    namespace.getQualifiedType(MockQualifiedTypeName1) shouldBe None
  }


}

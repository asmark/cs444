package joos.ast.expressions

import joos.ast.AstSpecHelper._
import joos.ast.exceptions.AstConstructionException
import joos.parsetree.LeafNode
import joos.tokens.{TerminalToken, TokenKind}
import org.scalatest.{Matchers, FlatSpec}
import joos.semantic.{BlockEnvironment, TypeEnvironment, ModuleEnvironment}

class ExpressionSpec extends FlatSpec with Matchers {

//  def withEnv(testCode: (ModuleEnvironment, TypeEnvironment, BlockEnvironment) => Any) {
//    implicit val moduleEnvironment: ModuleEnvironment = new ModuleEnvironment
//    implicit val typeEnvironment: TypeEnvironment = new TypeEnvironment
//    implicit val blockEnvironment: BlockEnvironment = BlockEnvironment(None)
//    testCode(moduleEnvironment, typeEnvironment, blockEnvironment)
//  }

  behavior of "SimpleNameExpression"
  it should "convert the parse tree into a SimpleNameExpression AST" in {
    implicit val moduleEnvironment: ModuleEnvironment = new ModuleEnvironment
    implicit val typeEnvironment: TypeEnvironment = new TypeEnvironment
    implicit val blockEnvironment: BlockEnvironment = BlockEnvironment(None)
    val astNode = SimpleNameExpression(simpleNameTree)
    astNode.identifier.kind shouldEqual TokenKind.Id
    astNode.identifier.lexeme shouldEqual "someId"
  }

  it should "fail when given an invalid parse tree" in {
    implicit val moduleEnvironment: ModuleEnvironment = new ModuleEnvironment
    implicit val typeEnvironment: TypeEnvironment = new TypeEnvironment
    implicit val blockEnvironment: BlockEnvironment = BlockEnvironment(None)
    intercept[AstConstructionException] {
      SimpleNameExpression(qualifiedNameTree)
    }
  }

  it should "fail when given an invalid token" in {
    intercept[AstConstructionException] {
      implicit val moduleEnvironment: ModuleEnvironment = new ModuleEnvironment
      implicit val typeEnvironment: TypeEnvironment = new TypeEnvironment
      implicit val blockEnvironment: BlockEnvironment = BlockEnvironment(None)

      SimpleNameExpression(LeafNode(TerminalToken(".", TokenKind.Dot)))
    }
  }

  behavior of "QualifiedNameExpression"
  it should "convert the parse tree into a QualifiedNameExpression" in {
    implicit val moduleEnvironment: ModuleEnvironment = new ModuleEnvironment
    implicit val typeEnvironment: TypeEnvironment = new TypeEnvironment
    implicit val blockEnvironment: BlockEnvironment = BlockEnvironment(None)
    val astNode = QualifiedNameExpression(qualifiedNameTree)
    astNode.name.identifier.lexeme shouldEqual "someId"
    astNode.qualifier match {
      case SimpleNameExpression(id) => id.lexeme shouldEqual "someId"
      case _ => fail
    }
  }

  it should "fail when given an invalid parse tree" in {
    intercept[AstConstructionException] {
      implicit val moduleEnvironment: ModuleEnvironment = new ModuleEnvironment
      implicit val typeEnvironment: TypeEnvironment = new TypeEnvironment
      implicit val blockEnvironment: BlockEnvironment = BlockEnvironment(None)
      QualifiedNameExpression(nameToQualifiedNameTree)
    }
  }

  behavior of "NameExpression"
  it should "convert the parse tree into a NameExpression AST with a SimpleName child" in {
    implicit val moduleEnvironment: ModuleEnvironment = new ModuleEnvironment
    implicit val typeEnvironment: TypeEnvironment = new TypeEnvironment
    implicit val blockEnvironment: BlockEnvironment = BlockEnvironment(None)
    val astNode = NameExpression(nameToSimpleNameTree)
    astNode match {
      case SimpleNameExpression(id) => id.lexeme shouldEqual "someId"
      case _ => fail
    }
  }

  it should "convert the parse tree into a NameExpression AST with a QualifiedName child" in {
    implicit val moduleEnvironment: ModuleEnvironment = new ModuleEnvironment
    implicit val typeEnvironment: TypeEnvironment = new TypeEnvironment
    implicit val blockEnvironment: BlockEnvironment = BlockEnvironment(None)
    val astNode = NameExpression(nameToQualifiedNameTree)
    astNode match {
      case QualifiedNameExpression(qualifier, name) => name.identifier.lexeme shouldEqual "someId"
      case _ => fail
    }
  }

  it should "fail when given an invalid parse tree" in {
    implicit val moduleEnvironment: ModuleEnvironment = new ModuleEnvironment
    implicit val typeEnvironment: TypeEnvironment = new TypeEnvironment
    implicit val blockEnvironment: BlockEnvironment = BlockEnvironment(None)
    intercept[AstConstructionException] {
      NameExpression(simpleNameTree)
    }
  }

}

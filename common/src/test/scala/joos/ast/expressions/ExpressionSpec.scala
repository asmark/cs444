package joos.ast.expressions

import joos.astspec._
import joos.parsetree.LeafNode
import joos.tokens.{TerminalToken, TokenKind}
import org.scalatest.{Matchers, FlatSpec}
import joos.ast.AstConstructionException

class ExpressionSpec extends FlatSpec with Matchers {

  behavior of "SimpleNameExpression"
  it should "convert the parse tree into a SimpleNameExpression AST" in {
    val astNode = SimpleNameExpression(simpleNameTree)
    astNode.identifier.kind shouldEqual TokenKind.Id
    astNode.identifier.lexeme shouldEqual "someId"
  }

  it should "fail when given an invalid parse tree" in {
    intercept[AstConstructionException] {
      SimpleNameExpression(qualifiedNameTree)
    }
  }

  it should "fail when given an invalid token" in {
    intercept[AstConstructionException] {
      SimpleNameExpression(LeafNode(TerminalToken(".", TokenKind.Dot)))
    }
  }

  behavior of "QualifiedNameExpression"
  it should "convert the parse tree into a QualifiedNameExpression" in {
    val astNode = QualifiedNameExpression(qualifiedNameTree)
    astNode.name.identifier.lexeme shouldEqual "someId"
    astNode.qualifier match {
      case SimpleNameExpression(id) => id.lexeme shouldEqual "someId"
      case _ => fail()
    }
  }

  it should "fail when given an invalid parse tree" in {
    intercept[AstConstructionException] {
      QualifiedNameExpression(nameToQualifiedNameTree)
    }
  }

  behavior of "NameExpression"
  it should "convert the parse tree into a NameExpression AST with a SimpleName child" in {
    val astNode = NameExpression(nameToSimpleNameTree)
    astNode match {
      case SimpleNameExpression(id) => id.lexeme shouldEqual "someId"
      case _ => fail()
    }
  }

  it should "convert the parse tree into a NameExpression AST with a QualifiedName child" in {
    val astNode = NameExpression(nameToQualifiedNameTree)
    astNode match {
      case QualifiedNameExpression(qualifier, name) => name.identifier.lexeme shouldEqual "someId"
      case _ => fail()
    }
  }

  it should "fail when given an invalid parse tree" in {
    intercept[AstConstructionException] {
      NameExpression(simpleNameTree)
    }
  }

}

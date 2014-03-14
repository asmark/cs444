package joos.ast.expressions

import joos.ast.compositions.TypedLike
import joos.ast.declarations.{MethodDeclaration, FieldDeclaration, TypeDeclaration}
import joos.ast.types.{SimpleType, ArrayType, PrimitiveType}
import joos.ast.{AstConstructionException, AstNode}
import joos.semantic.Declaration
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{LeafNode, TreeNode, ParseTreeNode}

trait Expression extends AstNode with TypedLike {
  override def declarationType = {
    require(declarationRef != null)
    declarationRef match {
      case Left(None) => PrimitiveType.BooleanType // Doesn't matter what type we give?
      case Left(Some(declaration)) => ArrayType(PrimitiveType.BooleanType) // Doesn't matter what type we give?
      case Right(declaration) => declaration match {
        case t:TypeDeclaration => SimpleType(NameExpression(t.fullName))
        case f: FieldDeclaration => f.declarationType
        case m: MethodDeclaration => m.returnType.get
      }
    }
  }

  private var _declarationRef: Declaration = null
  def declarationRef = _declarationRef
  def declarationRef_=(declarationRef: Declaration) = _declarationRef = declarationRef
}

object Expression {
  def apply(ptn: ParseTreeNode): Expression = {
    ptn match {
      // Recursive call to Expression
      case TreeNode(ProductionRule("Expression", Seq("AssignmentExpression")), _, children) =>
        Expression(children(0))
      case TreeNode(ProductionRule("AssignmentExpression", Seq("ConditionalExpression")), _, children) =>
        Expression(children(0))
      case TreeNode(ProductionRule("ConditionalExpression", Seq("ConditionalOrExpression")), _, children) =>
        Expression(children(0))
      case TreeNode(ProductionRule("ConditionalOrExpression", Seq("ConditionalAndExpression")), _, children) =>
        Expression(children(0))
      case TreeNode(ProductionRule("ConditionalAndExpression", Seq("InclusiveOrExpression")), _, children) =>
        Expression(children(0))
      case TreeNode(ProductionRule("InclusiveOrExpression", Seq("ExclusiveOrExpression")), _, children) =>
        Expression(children(0))
      case TreeNode(ProductionRule("ExclusiveOrExpression", Seq("AndExpression")), _, children) =>
        Expression(children(0))
      case TreeNode(ProductionRule("AndExpression", Seq("EqualityExpression")), _, children) =>
        Expression(children(0))
      case TreeNode(ProductionRule("EqualityExpression", Seq("RelationalExpression")), _, children) =>
        Expression(children(0))
      case TreeNode(ProductionRule("RelationalExpression", Seq("AdditiveExpression")), _, children) =>
        Expression(children(0))
      case TreeNode(ProductionRule("AdditiveExpression", Seq("MultiplicativeExpression")), _, children) =>
        Expression(children(0))
      case TreeNode(ProductionRule("MultiplicativeExpression", Seq("UnaryExpression")), _, children) =>
        Expression(children(0))
      case TreeNode(ProductionRule("UnaryExpression", Seq("UnaryExpressionNotPlusMinus")), _, children) =>
        Expression(children(0))
      case TreeNode(ProductionRule("UnaryExpressionNotPlusMinus", Seq("Primary")), _, children) =>
        Expression(children(0))
      case TreeNode(ProductionRule("Primary", Seq("PrimaryNoNewArray")), _, children) =>
        Expression(children(0))
      case TreeNode(ProductionRule("Primary", Seq("ArrayCreationExpression")), _, children) =>
        ArrayCreationExpression(children(0))
      // Concrete Expressions
      case TreeNode(ProductionRule(_, Seq("Name")), _, children) =>
        NameExpression(children(0))
      case TreeNode(ProductionRule("UnaryExpressionNotPlusMinus", Seq("CastExpression")), _, children) =>
        CastExpression(children(0))
      case TreeNode(ProductionRule("LeftHandSide", Seq("FieldAccess")), _, children) =>
        FieldAccessExpression(children(0))
      case TreeNode(ProductionRule("LeftHandSide", Seq("ArrayAccess")), _, children) =>
        ArrayAccessExpression(children(0))
      case TreeNode(ProductionRule("AssignmentExpression", Seq("Assignment")), _, children) =>
        AssignmentExpression(children(0))
      case TreeNode(
      ProductionRule(
      "ConditionalOrExpression" | "ConditionalAndExpression" | "InclusiveOrExpression" |
      "ExclusiveOrExpression" | "AndExpression" | "EqualityExpression" | "RelationalExpression" |
      "AdditiveExpression" | "MultiplicativeExpression", _), _, children) => {
        children match {
          case Seq(singleChild) =>
            Expression(singleChild)
          case Seq(left, LeafNode(operator), right) => {
            right match {
              case TreeNode(ProductionRule("ReferenceType", _), _, _) =>
                InstanceOfExpression(ptn)
              case _ =>
                InfixExpression(ptn)
            }
          }
        }
      }
      case TreeNode(ProductionRule(_, Seq(_, "UnaryExpression")), _, children) =>
        PrefixExpression(ptn)
      case TreeNode(ProductionRule("PrimaryNoNewArray", Seq("Literal")), _, children) =>
        LiteralExpression(children(0))
      case TreeNode(ProductionRule("PrimaryNoNewArray", Seq("ClassInstanceCreationExpression")), _, children) =>
        ClassInstanceCreationExpression(children(0))
      case TreeNode(ProductionRule("PrimaryNoNewArray", Seq("(", "Expression", ")")), _, children) =>
        ParenthesizedExpression(ptn)
      case TreeNode(ProductionRule("PrimaryNoNewArray", Seq("FieldAccess")), _, children) =>
        FieldAccessExpression(children(0))
      case TreeNode(ProductionRule("PrimaryNoNewArray", Seq("MethodInvocation")), _, children) =>
        MethodInvocationExpression(children(0))
      case TreeNode(ProductionRule("PrimaryNoNewArray", Seq("ArrayAccess")), _, children) =>
        ArrayAccessExpression(children(0))
      case TreeNode(ProductionRule("PrimaryNoNewArray", Seq("this")), _, children) =>
        ThisExpression(children(0))
      case TreeNode(ProductionRule("Name", _), _, _) =>
        NameExpression(ptn)
      case _ => throw new AstConstructionException("Invalid tree node to create Expression")
    }
  }

  def argList(ptn: ParseTreeNode): Seq[Expression] = {
    ptn match {
      case TreeNode(ProductionRule("ArgumentList", Seq("Expression")), _, children) =>
        Seq(Expression(children(0)))
      case TreeNode(ProductionRule("ArgumentList", _), _, children) =>
        argList(children(0)) :+ Expression(children(2))
      case _ => throw new AstConstructionException("No valid production rule to make an ArgumentList")
    }
  }
}
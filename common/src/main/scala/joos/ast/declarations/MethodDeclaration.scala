package joos.ast.declarations

import joos.ast.exceptions.AstConstructionException
import joos.ast.expressions.SimpleNameExpression
import joos.ast.{Block, Type, Modifier}
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.semantic.{TypeEnvironment, BlockEnvironment, ModuleEnvironment}

case class MethodDeclaration(
    modifiers: Seq[Modifier],
    returnType: Option[Type],
    returnDims: Int, // TODO: remove this attribute?
    name: SimpleNameExpression,
    params: Seq[SingleVariableDeclaration],
    body: Option[Block],
    isConstructor: Boolean)(
    implicit val moduleEnvironment: ModuleEnvironment,
    typeEnvironment: TypeEnvironment) extends BodyDeclaration

object MethodDeclaration {
  private def handleMethodHeader(ptn: ParseTreeNode) = {

  }

  private def handleMethodBody(ptn: ParseTreeNode)(
      implicit moduleEnvironment: ModuleEnvironment,
      typeEnvironment: TypeEnvironment,
      environment: BlockEnvironment): Option[Block] = {
    ptn match {
      case TreeNode(ProductionRule("MethodBody", Seq("Block")), _, children) =>
        return Some(Block(children(0)))
      case TreeNode(ProductionRule("MethodBody", Seq(";")), _, children) =>
        return None
      case _ => throw new AstConstructionException("No valid production rule to create MethodBody")
    }
  }

  def apply(ptn: ParseTreeNode)(
      implicit moduleEnvironment: ModuleEnvironment, typeEnvironment: TypeEnvironment): MethodDeclaration = {

    implicit val environment = BlockEnvironment(Some(typeEnvironment))

    val method = ptn match {
      case TreeNode(ProductionRule("MethodDeclaration", Seq("MethodHeader", "MethodBody")), _, children) => {
        val header = children(0)
        val body = handleMethodBody(children(1))(moduleEnvironment, typeEnvironment, environment)

        header match {
          case TreeNode(
          ProductionRule("MethodHeader", Seq("Modifiers", typeString, "MethodDeclarator")),
          _,
          children
          ) => {
            val modifiers = Modifier(children(0))
            val returnType: Option[Type] = if (typeString.equals("Type")) Some(Type(children(1))) else None
            val methodDeclaratorNode = children(2)

            methodDeclaratorNode match {
              case TreeNode(
              ProductionRule("MethodDeclarator", Seq("Identifier", "(", "FormalParameterList", ")")),
              _,
              children
              ) => {
                val name = SimpleNameExpression(children(0))
                val params = SingleVariableDeclaration.createFormalParameterNodes(children(2))
                new MethodDeclaration(modifiers, returnType, -1, name, params, body, false)
              }

              case TreeNode(
              ProductionRule("MethodDeclarator", Seq("Identifier", "(", ")")),
              _,
              children
              ) => {
                val name = SimpleNameExpression(children(0))
                new MethodDeclaration(modifiers, returnType, -1, name, Seq(), body, false)
              }

              case _ => throw new AstConstructionException("No valid production rule to create MethodHeader")
            }
          }
          case _ => throw new AstConstructionException("No valid production rule to create MethodHeader")
        }
      }

      case TreeNode(
      ProductionRule("ConstructorDeclaration", Seq("Modifiers", "ConstructorDeclarator", "ConstructorBody")),
      _,
      children
      ) => {
        val modifiers = Modifier(children(0))
        val constructorDeclaratorNode = children(1)
        val body = Some(Block(children(2)))

        constructorDeclaratorNode match {
          case TreeNode(
          ProductionRule("ConstructorDeclarator", Seq("SimpleName", "(", "FormalParameterList", ")")),
          _,
          children
          ) => {
            val name = SimpleNameExpression(children(0).children(0))
            val params = SingleVariableDeclaration.createFormalParameterNodes(children(2))
            new MethodDeclaration(modifiers, None, -1, name, params, body, true)
          }

          case TreeNode(
          ProductionRule("ConstructorDeclarator", Seq("SimpleName", "(", ")")),
          _,
          children
          ) => {
            val name = SimpleNameExpression(children(0).children(0))
            new MethodDeclaration(modifiers, None, -1, name, Seq(), body, true)
          }

          case _ => throw new AstConstructionException("No valid production rule to create ConstructorDeclarator")
        }

      }

      case TreeNode(
      ProductionRule("AbstractMethodDeclaration", Seq("MethodHeader", ";")),
      _,
      children
      ) => {
        val header = children(0)
        val body = None

        header match {
          case TreeNode(
          ProductionRule("MethodHeader", Seq("Modifiers", typeString, "MethodDeclarator")),
          _,
          children
          ) => {
            val modifiers = Modifier(children(0))
            val returnType: Option[Type] = if (typeString.equals("Type")) Some(Type(children(1))) else None
            val methodDeclaratorNode = children(2)

            methodDeclaratorNode match {
              case TreeNode(
              ProductionRule("MethodDeclarator", Seq("Identifier", "(", "FormalParameterList", ")")),
              _,
              children
              ) => {
                val name = SimpleNameExpression(children(0))
                val params = SingleVariableDeclaration.createFormalParameterNodes(children(2))
                new MethodDeclaration(modifiers, returnType, -1, name, params, body, false)
              }

              case TreeNode(
              ProductionRule("MethodDeclarator", Seq("Identifier", "(", ")")),
              _,
              children
              ) => {
                val name = SimpleNameExpression(children(0))
                new MethodDeclaration(modifiers, returnType, -1, name, Seq(), body, false)
              }

              case _ => throw new AstConstructionException("No valid production rule to create MethodHeader")
            }
          }
        }
      }

      case _ => throw new AstConstructionException("No valid production rule to create MethodDeclaration")
    }

    method.params.foreach(environment.add)
    method
  }
}

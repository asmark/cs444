package joos.ast.declarations

import joos.ast._
import joos.ast.compositions.{BlockLike, DeclarationLike}
import joos.ast.expressions.{NameExpression, SimpleNameExpression}
import joos.ast.statements.Block
import joos.ast.types.PrimitiveType
import joos.ast.types.{SimpleType, Type, ArrayType}
import joos.semantic.BlockEnvironment
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.ParseTreeNode
import joos.syntax.parsetree.TreeNode

case class MethodDeclaration(
    modifiers: Seq[Modifier],
    returnType: Option[Type],
    name: SimpleNameExpression,
    parameters: IndexedSeq[SingleVariableDeclaration],
    body: Option[Block],
    isConstructor: Boolean)
    extends BodyDeclaration with DeclarationLike with BlockLike {
  var compilationUnit: CompilationUnit = null
  var typeDeclaration: TypeDeclaration = null
  var environment: BlockEnvironment = null


  override def toString = {
    val returnTypeString = returnType match {
      case None => ""
      case Some(t) => t.standardName
    }
    val parametersString = parameters.mkString(", ")

    val bodyText = body match {
      case None => ";\n"
      case Some(block) => s"\n${block}\n"
    }

    s"${modifiers.mkString(" ")} ${returnTypeString} ${name} (${parametersString}) ${bodyText}"
  }

  /**
   * Method signature with argument types added
   */
  lazy val typedSignature = parameters.foldLeft(name.standardName) {
    (result, parameter) =>
      val name = result + '-' + getTypeName(parameter.variableType)
      parameter.variableType match {
        case _: ArrayType => name + "[]"
        case _ => name
      }
  }

  lazy val returnTypeLocalSignature = {
    var mods: String = null
    this.modifiers.foreach(mod => mods += mod.name + " ")
    mods + (returnType match {
      case Some(someType) => getTypeName(someType) + " " + localSignature
      case None => localSignature
    })
  }

  lazy val localSignature = {
    val localMethodName = name match {
      case simpleNameExpression: SimpleNameExpression => {
        simpleNameExpression.standardName
      }
    }

    parameters.foldLeft(localMethodName) {
      (result, parameter) =>
        val name = result + '-' + getTypeName(parameter.variableType)
        parameter.variableType match {
          case _: ArrayType => name + "[]"
          case _ => name
        }
    }
  }

  lazy val isAbstractMethod: Boolean = {
    require(typeDeclaration != null)
    (modifiers contains Modifier.Abstract) || typeDeclaration.isInterface
  }

  private[this] def getTypeName(t: Type): String = {
    t match {
      case t: PrimitiveType => t.name
      case ArrayType(x, _) => getTypeName(x)
      case SimpleType(name) => {
        val typeDeclaration = compilationUnit.getVisibleType(name).get
        typeDeclaration.packageDeclaration.name.standardName + '.' + typeDeclaration.name.standardName
      }
    }
  }

  override def declarationName: NameExpression = name
}

object MethodDeclaration {
  private def handleMethodBody(ptn: ParseTreeNode): Option[Block] = {
    ptn match {
      case TreeNode(ProductionRule("MethodBody", Seq("Block")), _, children) =>
        Some(Block(children(0)))
      case TreeNode(ProductionRule("MethodBody", Seq(";")), _, children) =>
        None
      case _ => throw new AstConstructionException("No valid production rule to create MethodBody")
    }
  }

  def apply(ptn: ParseTreeNode): MethodDeclaration = {
    ptn match {
      case TreeNode(ProductionRule("MethodDeclaration", Seq("MethodHeader", "MethodBody")), _, children) => {
        val header = children(0)
        val body = handleMethodBody(children(1))

        header match {
          case TreeNode(
          ProductionRule("MethodHeader", Seq("Modifiers", typeString, "MethodDeclarator")),
          _,
          children
          ) => {
            val modifiers = Modifier(children(0))
            val returnType: Option[Type] = if (typeString.equals("Type")) Some(Type(children(1))) else Some(PrimitiveType.VoidType)
            val methodDeclaratorNode = children(2)

            methodDeclaratorNode match {
              case TreeNode(
              ProductionRule("MethodDeclarator", Seq("Identifier", "(", "FormalParameterList", ")")),
              _,
              children
              ) => {
                val name = SimpleNameExpression(children(0))
                val params = SingleVariableDeclaration.createFormalParameterNodes(children(2))
                new MethodDeclaration(modifiers, returnType, name, params, body, false)
              }

              case TreeNode(
              ProductionRule("MethodDeclarator", Seq("Identifier", "(", ")")),
              _,
              children
              ) => {
                val name = SimpleNameExpression(children(0))
                new MethodDeclaration(modifiers, returnType, name, IndexedSeq(), body, false)
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
            new MethodDeclaration(modifiers, None, name, params, body, true)
          }

          case TreeNode(
          ProductionRule("ConstructorDeclarator", Seq("SimpleName", "(", ")")),
          _,
          children
          ) => {
            val name = SimpleNameExpression(children(0).children(0))
            new MethodDeclaration(modifiers, None, name, IndexedSeq(), body, true)
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
            val returnType: Option[Type] = if (typeString.equals("Type")) Some(Type(children(1))) else Some(PrimitiveType.VoidType)
            val methodDeclaratorNode = children(2)

            methodDeclaratorNode match {
              case TreeNode(
              ProductionRule("MethodDeclarator", Seq("Identifier", "(", "FormalParameterList", ")")),
              _,
              children
              ) => {
                val name = SimpleNameExpression(children(0))
                val params = SingleVariableDeclaration.createFormalParameterNodes(children(2))
                new MethodDeclaration(modifiers, returnType, name, params, body, false)
              }

              case TreeNode(
              ProductionRule("MethodDeclarator", Seq("Identifier", "(", ")")),
              _,
              children
              ) => {
                val name = SimpleNameExpression(children(0))
                new MethodDeclaration(modifiers, returnType, name, IndexedSeq(), body, false)
              }

              case _ => throw new AstConstructionException("No valid production rule to create MethodHeader")
            }
          }
        }
      }

      case _ => throw new AstConstructionException("No valid production rule to create MethodDeclaration")
    }
  }
}

package joos.ast.declarations

import joos.ast._
import joos.ast.expressions.SimpleNameExpression
import joos.ast.statements.Block
import joos.ast.types.{PrimitiveType, SimpleType, Type, ArrayType}
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.ParseTreeNode
import joos.syntax.parsetree.TreeNode
import joos.semantic.BlockEnvironment

case class MethodDeclaration(
    modifiers: Seq[Modifier],
    returnType: Option[Type],
    name: SimpleNameExpression,
    parameters: IndexedSeq[SingleVariableDeclaration],
    body: Option[Block],
    isConstructor: Boolean)
    extends BodyDeclaration {
  var compilationUnit: CompilationUnit = null
  var typeDeclaration: TypeDeclaration = null
  var environment: BlockEnvironment = null


  override def toString = {
    val returnTypeString = returnType match {
      case None => "void"
      case Some(t) => t.standardName
    }
    val parametersString = parameters.foldLeft("") {
      (result, parameter) =>
        result + parameter.declarationType.standardName + ' ' + parameter.declarationName.standardName
    }

    List(modifiers.mkString(" "), returnTypeString, name.standardName, "(", parametersString, ")").mkString(" ")
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
    this.modifiers.foreach(mod => mods += mod.modifier.lexeme + " ")
    mods + (returnType match {
      case Some(someType) => getTypeName(someType) + " " + localSignature
      case None => "void " + localSignature
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
    (modifiers contains Modifier.Abstract) || typeDeclaration.isInterface
  }

  private[this] def getTypeName(t: Type): String = {
    t match {
      case PrimitiveType(token) => token.lexeme
      case ArrayType(x, _) => getTypeName(x)
      case SimpleType(name) => {
        val typeDeclaration = compilationUnit.getVisibleType(name).get
        typeDeclaration.packageDeclaration.name.standardName + '.' + typeDeclaration.name.standardName
      }
    }
  }
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

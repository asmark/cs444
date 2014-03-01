package joos.ast.declarations

import joos.ast.exceptions.AstConstructionException
import joos.ast.expressions.{SimpleNameExpression, NameExpression}
import joos.ast.{CompilationUnit, Modifier}
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.semantic.TypeEnvironment

case class TypeDeclaration(
    modifiers: Seq[Modifier],
    isInterface: Boolean,
    name: SimpleNameExpression,
    superType: Option[NameExpression], // TODO: Change to Option[SimpleType]??
    superInterfaces: Seq[NameExpression], // TODO: Change to Option[Seq[SimpleType]]??
    fields: Seq[FieldDeclaration],
    methods: Seq[MethodDeclaration]) extends BodyDeclaration with TypeEnvironment {
  var compilationUnit: CompilationUnit = null
  var packageDeclaration: PackageDeclaration = null
}

object TypeDeclaration {
  private def createInterfaceNodes(ptn: ParseTreeNode): Seq[NameExpression] = {
    ptn match {
      case TreeNode(ProductionRule("InterfaceTypeList", Seq("InterfaceType")), _, children) =>
        Seq(NameExpression(children(0).children(0).children(0)))
      case TreeNode(ProductionRule("InterfaceTypeList", Seq("InterfaceTypeList", ",", "InterfaceType")), _, children) =>
        createInterfaceNodes(children(0)) ++ Seq(NameExpression(children(2).children(0).children(0)))
      case _ => throw new AstConstructionException("No valid production rule to create InterfaceTypeList")
    }
  }

  private def handleClassBodyDeclaration(
      ptn: ParseTreeNode
      ): (Seq[FieldDeclaration], Seq[MethodDeclaration]) = {
    var fields: Seq[FieldDeclaration] = Seq()
    var methods: Seq[MethodDeclaration] = Seq()
    ptn match {
      case TreeNode(ProductionRule("ClassBodyDeclarations", Seq("ClassBodyDeclaration")), _, children) =>
        handleClassBodyDeclaration(children(0))

      case TreeNode(ProductionRule("ClassBodyDeclaration", Seq("ClassMemberDeclaration")), _, children) =>
        children(0) match {
          case TreeNode(ProductionRule("ClassMemberDeclaration", Seq("FieldDeclaration")), _, children) => {
            if (fields.equals(None)) {
              fields = Seq(FieldDeclaration(children(0)))
            } else {
              fields ++= Seq(FieldDeclaration(children(0)))
            }
            return (fields, methods)
          }
          case TreeNode(ProductionRule("ClassMemberDeclaration", Seq("MethodDeclaration")), _, children) => {
            if (methods.equals(None)) {
              methods = Seq(MethodDeclaration(children(0)))
            } else {
              methods ++= Seq(MethodDeclaration(children(0)))
            }
            return (fields, methods)
          }
          case _ => throw new AstConstructionException("No valid production rule to create ClassMemberDeclaration")
        }

      case TreeNode(ProductionRule("ClassBodyDeclaration", Seq("ConstructorDeclaration")), _, children) => {
        if (methods.equals(None)) {
          methods = Seq(MethodDeclaration(children(0)))
        } else {
          methods ++= Seq(MethodDeclaration(children(0)))
        }
        return (fields, methods)
      }

      case TreeNode(
      ProductionRule("ClassBodyDeclarations", Seq("ClassBodyDeclarations", "ClassBodyDeclaration")),
      _,
      children
      ) => {
        val more = handleClassBodyDeclaration(children(0))
        val rightMost = handleClassBodyDeclaration(children(1))
        return (more._1 ++ rightMost._1, more._2 ++ rightMost._2)
      }

      case _ => throw new AstConstructionException("No valid production rule to create ClassBodyDeclarations")
    }
  }

  private def handleClassDeclaration(ptn: ParseTreeNode): TypeDeclaration = {
    ptn match {
      case TreeNode(
      ProductionRule("ClassDeclaration", Seq("Modifiers", _, "Identifier", "SuperClause", "Interfaces", "ClassBody")),
      _,
      children
      ) => {
        val classBody = children(5)
        var declarations: (Seq[FieldDeclaration], Seq[MethodDeclaration]) = (Seq(), Seq())

        classBody match {
          case TreeNode(ProductionRule("ClassBody", Seq("{", "ClassBodyDeclarations", "}")), _, children) => {
            declarations = handleClassBodyDeclaration(children(1))
          }
          case TreeNode(ProductionRule("ClassBody", Seq("{", "}")), _, children) => {}
          case _ => throw new AstConstructionException("No valid production rule to create ClassBody")
        }

        return new TypeDeclaration(
          Modifier(children(0)),
          false,
          SimpleNameExpression(children(2)),
          Some(NameExpression(children(3).children(1).children(0).children(0))),
          createInterfaceNodes(children(4).children(1)),
          declarations._1,
          declarations._2
        )
      }

      case TreeNode(
      ProductionRule("ClassDeclaration", Seq("Modifiers", _, "Identifier", "SuperClause", "ClassBody")),
      _,
      children
      ) => {
        val classBody = children(4)
        var declarations: (Seq[FieldDeclaration], Seq[MethodDeclaration]) = (Seq(), Seq())

        classBody match {
          case TreeNode(ProductionRule("ClassBody", Seq("{", "ClassBodyDeclarations", "}")), _, children) => {
            declarations = handleClassBodyDeclaration(children(1))
          }
          case TreeNode(ProductionRule("ClassBody", Seq("{", "}")), _, children) => {}
          case _ => throw new AstConstructionException("No valid production rule to create ClassBody")
        }

        return new TypeDeclaration(
          Modifier(children(0)),
          false,
          SimpleNameExpression(children(2)),
          Some(NameExpression(children(3).children(1).children(0).children(0))),
          Seq(),
          declarations._1,
          declarations._2
        )
      }

      case TreeNode(
      ProductionRule("ClassDeclaration", Seq("Modifiers", _, "Identifier", "Interfaces", "ClassBody")),
      _,
      children
      ) => {
        val classBody = children(4)
        var declarations: (Seq[FieldDeclaration], Seq[MethodDeclaration]) = (Seq(), Seq())

        classBody match {
          case TreeNode(ProductionRule("ClassBody", Seq("{", "ClassBodyDeclarations", "}")), _, children) => {
            declarations = handleClassBodyDeclaration(children(1))
          }
          case TreeNode(ProductionRule("ClassBody", Seq("{", "}")), _, children) => {}
          case _ => throw new AstConstructionException("No valid production rule to create ClassBody")
        }

        return new TypeDeclaration(
          Modifier(children(0)),
          false,
          SimpleNameExpression(children(2)),
          None,
          createInterfaceNodes(children(3).children(1)),
          declarations._1,
          declarations._2
        )
      }

      case TreeNode(
      ProductionRule("ClassDeclaration", Seq("Modifiers", _, "Identifier", "ClassBody")),
      _,
      children
      ) => {
        val classBody = children(3)
        var declarations: (Seq[FieldDeclaration], Seq[MethodDeclaration]) = (Seq(), Seq())

        classBody match {
          case TreeNode(ProductionRule("ClassBody", Seq("{", "ClassBodyDeclarations", "}")), _, children) => {
            declarations = handleClassBodyDeclaration(children(1))
          }
          case TreeNode(ProductionRule("ClassBody", Seq("{", "}")), _, children) => {}
          case _ => throw new AstConstructionException("No valid production rule to create ClassBody")
        }

        return new TypeDeclaration(
          Modifier(children(0)),
          false,
          SimpleNameExpression(children(2)),
          None,
          Seq(),
          declarations._1,
          declarations._2
        )
      }

      case _ => throw new AstConstructionException("No valid production rule to create ClassDeclaration")
    }
  }

  private def createExtendedInterfaceNodes(ptn: ParseTreeNode): Seq[NameExpression] = {
    ptn match {
      case TreeNode(ProductionRule("ExtendsInterfaces", Seq("extends", "InterfaceType")), _, children) => {
        return Seq(NameExpression(children(1).children(0).children(0)))
      }

      case TreeNode(
      ProductionRule("ExtendsInterfaces", Seq("ExtendsInterfaces", ",", "InterfaceType")),
      _,
      children)
      => {
        return createExtendedInterfaceNodes(children(0)) ++ Seq(NameExpression(children(2).children(0).children(0)))
      }

      case _ => throw new AstConstructionException("No valid production rule to create ExtendsInterfaces")
    }
  }

  private def createInterfaceMethodNodes(ptn: ParseTreeNode): Seq[MethodDeclaration] = {
    ptn match {
      case TreeNode(ProductionRule("InterfaceMemberDeclarations", Seq("AbstractMethodDeclaration")), _, children) => {
        return Seq(MethodDeclaration(children(0)))
      }
      case TreeNode(
      ProductionRule(
      "InterfaceMemberDeclarations",
      Seq("InterfaceMemberDeclarations", "AbstractMethodDeclaration")), _, children) => {
        return createInterfaceMethodNodes(children(0)) ++ Seq(MethodDeclaration(children(1)))
      }
      case _ => throw new AstConstructionException("No valid production rule to create InterfaceMemberDeclarations")
    }
  }

  private def handleInterfaceBody(ptn: ParseTreeNode): Seq[MethodDeclaration] = {
    ptn match {
      case TreeNode(ProductionRule("InterfaceBody", Seq("{", "InterfaceMemberDeclarations", "}")), _, children) => {
        return createInterfaceMethodNodes(children(1))
      }

      case TreeNode(ProductionRule("InterfaceBody", Seq("{", "}")), _, children) => {
        return Seq()
      }

      case _ => throw new AstConstructionException("No valid production rule to create InterfaceBody")
    }
  }

  private def handleInterfaceDeclaration(ptn: ParseTreeNode): TypeDeclaration = {
    ptn match {
      case TreeNode(
      ProductionRule(
      "InterfaceDeclaration",
      Seq("Modifiers", "interface", "Identifier", "ExtendsInterfaces", "InterfaceBody")),
      _,
      children
      ) => {
        return TypeDeclaration(
          Modifier(children(0)),
          true,
          SimpleNameExpression(children(2)),
          None,
          createExtendedInterfaceNodes(children(3)),
          Seq(),
          handleInterfaceBody(children(4))
        )
      }

      case TreeNode(
      ProductionRule("InterfaceDeclaration", Seq("Modifiers", "interface", "Identifier", "InterfaceBody")),
      _,
      children
      ) => {
        return TypeDeclaration(
          Modifier(children(0)),
          true,
          SimpleNameExpression(children(2)),
          None,
          Seq(),
          Seq(),
          handleInterfaceBody(children(3))
        )
      }

      case _ => throw new AstConstructionException("No valid production rule to create InterfaceDeclaration")
    }
  }

  def apply(ptn: ParseTreeNode): TypeDeclaration = {
    ptn match {
      case TreeNode(ProductionRule("TypeDeclaration", Seq("ClassDeclaration")), _, children) =>
        return handleClassDeclaration(children(0))
      case TreeNode(ProductionRule("TypeDeclaration", Seq("InterfaceDeclaration")), _, children) =>
        return handleInterfaceDeclaration(children(0))
      case TreeNode(ProductionRule("TypeDeclaration", Seq(";")), _, children) =>
        return null
      case _ => throw new AstConstructionException("No valid production rule to create TypeDeclaration")
    }
  }
}

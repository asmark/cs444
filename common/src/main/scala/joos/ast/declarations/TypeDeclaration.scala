package joos.ast.declarations

import joos.ast.Modifier
import joos.ast.compositions.{BlockLike, DeclarationLike}
import joos.ast.expressions.{SimpleNameExpression, NameExpression}
import joos.ast.types.SimpleType
import joos.ast.{AstConstructionException, CompilationUnit}
import joos.core.{Identifiable, Logger}
import joos.semantic.{BlockEnvironment, TypeEnvironment}
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}

case class TypeDeclaration(
    modifiers: Seq[Modifier],
    isInterface: Boolean,
    name: SimpleNameExpression,
    superType: Option[NameExpression],
    superInterfaces: Seq[NameExpression],
    fields: Seq[FieldDeclaration],
    methods: Seq[MethodDeclaration])
    extends BodyDeclaration
    with TypeEnvironment with DeclarationLike with BlockLike with Identifiable {
  implicit var compilationUnit: CompilationUnit = null
  var packageDeclaration: PackageDeclaration = null

  blockEnvironment = BlockEnvironment()(this)

  lazy val isConcreteClass = !isInterface && !(modifiers contains Modifier.Abstract)
  lazy val fullName = {
    if (packageDeclaration == null) {
      Logger.logWarning("Attempting to find FullName of a TypeDeclaration without a linked package declaration")
      name.standardName
    } else {
      if (packageDeclaration == PackageDeclaration.DefaultPackage) {
        name.standardName
      } else {
        s"${packageDeclaration.name.standardName}.${name.standardName}"
      }
    }
  }

  lazy val asType = {
    val simple = SimpleType(NameExpression(fullName))
    simple.declaration = this
    simple
  }

  override def equals(that: Any) = {
    that match {
      case that: TypeDeclaration => that.fullName == fullName
      case _ => false
    }
  }

  def toInterface: TypeDeclaration = {
    require(compilationUnit != null)
    require(packageDeclaration != null)
    val methods = this.methods.map {
      method =>
        new MethodDeclaration(
          Seq(Modifier.Abstract, Modifier.Public),
          method.returnType,
          name,
          method.parameters,
          None,
          method.isConstructor
        )
    }

    val objectInterface = new TypeDeclaration(
      Seq(Modifier.Public, Modifier.Abstract),
      isInterface = true,
      name,
      None,
      superInterfaces,
      Seq(),
      methods
    )
    objectInterface.compilationUnit = compilationUnit
    objectInterface.packageDeclaration = packageDeclaration
    objectInterface
  }

  override def declarationName: NameExpression = name
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
            (fields, methods)
          }
          case TreeNode(ProductionRule("ClassMemberDeclaration", Seq("MethodDeclaration")), _, children) => {
            if (methods.equals(None)) {
              methods = Seq(MethodDeclaration(children(0)))
            } else {
              methods ++= Seq(MethodDeclaration(children(0)))
            }
            (fields, methods)
          }
          case _ => throw new AstConstructionException("No valid production rule to create ClassMemberDeclaration")
        }

      case TreeNode(ProductionRule("ClassBodyDeclaration", Seq("ConstructorDeclaration")), _, children) => {
        if (methods.equals(None)) {
          methods = Seq(MethodDeclaration(children(0)))
        } else {
          methods ++= Seq(MethodDeclaration(children(0)))
        }
        (fields, methods)
      }

      case TreeNode(
      ProductionRule("ClassBodyDeclarations", Seq("ClassBodyDeclarations", "ClassBodyDeclaration")),
      _,
      children
      ) => {
        val more = handleClassBodyDeclaration(children(0))
        val rightMost = handleClassBodyDeclaration(children(1))
        (more._1 ++ rightMost._1, more._2 ++ rightMost._2)
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

        new TypeDeclaration(
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

        new TypeDeclaration(
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

        new TypeDeclaration(
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

        new TypeDeclaration(
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
        Seq(NameExpression(children(1).children(0).children(0)))
      }

      case TreeNode(
      ProductionRule("ExtendsInterfaces", Seq("ExtendsInterfaces", ",", "InterfaceType")),
      _,
      children)
      => {
        createExtendedInterfaceNodes(children(0)) ++ Seq(NameExpression(children(2).children(0).children(0)))
      }

      case _ => throw new AstConstructionException("No valid production rule to create ExtendsInterfaces")
    }
  }

  private def createInterfaceMethodNodes(ptn: ParseTreeNode): Seq[MethodDeclaration] = {
    ptn match {
      case TreeNode(ProductionRule("InterfaceMemberDeclarations", Seq("AbstractMethodDeclaration")), _, children) => {
        Seq(MethodDeclaration(children(0)))
      }
      case TreeNode(
      ProductionRule(
      "InterfaceMemberDeclarations",
      Seq("InterfaceMemberDeclarations", "AbstractMethodDeclaration")), _, children) => {
        createInterfaceMethodNodes(children(0)) ++ Seq(MethodDeclaration(children(1)))
      }
      case _ => throw new AstConstructionException("No valid production rule to create InterfaceMemberDeclarations")
    }
  }

  private def handleInterfaceBody(ptn: ParseTreeNode): Seq[MethodDeclaration] = {
    ptn match {
      case TreeNode(ProductionRule("InterfaceBody", Seq("{", "InterfaceMemberDeclarations", "}")), _, children) => {
        createInterfaceMethodNodes(children(1))
      }

      case TreeNode(ProductionRule("InterfaceBody", Seq("{", "}")), _, children) => {
        Seq()
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
        TypeDeclaration(
          Modifier(children(0)),
          isInterface = true,
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
        TypeDeclaration(
          Modifier(children(0)),
          isInterface = true,
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
        handleClassDeclaration(children(0))
      case TreeNode(ProductionRule("TypeDeclaration", Seq("InterfaceDeclaration")), _, children) =>
        handleInterfaceDeclaration(children(0))
      case _ => throw new AstConstructionException("No valid production rule to create TypeDeclaration")
    }
  }
}

package joos.weeder

import joos.parsetree._
import scala.collection.mutable
import joos.weeder.ExplicitClassConstructorWeeder
import joos.weeder.MethodWeeder
import joos.weeder.CastExpressionWeeder
import joos.parsetree.LeafNode
import joos.weeder.DecimalIntegerRangeWeeder
import joos.weeder.FieldWeeder
import joos.parsetree.TreeNode
import joos.weeder.ClassModifierWeeder
import joos.weeder.InterfaceMethodWeeder
import joos.parser.ParseMetaData

abstract class Weeder {
  // TODO: Get the name from tokenkind?
  final val Modifier = "Modifier"
  final val Modifiers = "Modifiers"
  final val Abstract = "abstract"
  final val Final = "final"
  final val Native = "native"
  final val MethodDeclaration = "MethodDeclaration"
  final val Block = "Block"
  final val Static = "static"
  final val AbstractMethodDeclaration = "AbstractMethodDeclaration"
  final val ClassBodyDeclarations = "ClassBodyDeclarations"
  final val ClassBodyDeclaration = "ClassBodyDeclaration"
  final val ConstructorDeclaration = "ConstructorDeclaration"
  final val ClassDeclaration = "ClassDeclaration"
  final val ClassBody = "ClassBody"
  final val FieldDeclaration = "FieldDeclaration"
  final val CastExpression = "CastExpression"
  final val ReferenceType = "ReferenceType"
  final val Primary = "Primary"
  final val Expression = "Expression"
  final val InterfaceDeclaration = "InterfaceDeclaration"
  final val UnaryExpression = "UnaryExpression"
  final val Public = "public"
  final val Protected = "protected"

  def check(ptn: ParseTreeNode, md: ParseMetaData)

  def getAllModifiers(modifiersNode: ParseTreeNode): Set[String] = {
    var curNode: ParseTreeNode = modifiersNode
    var ret = Set[String]()

    while (curNode.token.symbol.equals(Modifiers)) {
      val children = curNode.children
      // Always add the right most modifier
      if (children.length.equals(2))
        ret += children.last.children.head.token.symbol
      curNode = children.head
    }

    // Add left most modifier
    curNode.token.symbol match {
      case Modifier => ret += curNode.children.head.token.symbol
      case _ => {}
    }

    ret
  }

  def getAllClassBodyDeclaration(classBodyDeclarationsNode: ParseTreeNode): Set[String] = {
    var curNode: ParseTreeNode = classBodyDeclarationsNode
    var ret = Set[String]()

    while (curNode.token.symbol.equals(ClassBodyDeclarations)) {
      val children = curNode.children
      // Always add the right most modifier
      if (children.length.equals(2)) {
        ret += children.last.children.head.token.symbol
      }
      curNode = children.head
    }

    // Add left most modifier
    curNode.token.symbol match {
      case ClassBodyDeclaration => ret += curNode.children.head.token.symbol
      case _ => {}
    }

    ret
  }
}

object Weeder {
  def getWeeders = {
    Seq(
      ClassModifierWeeder(),
      MethodWeeder(),
      InterfaceMethodWeeder(),
      ExplicitClassConstructorWeeder(),
      FieldWeeder(),
      DecimalIntegerRangeWeeder(),
      CastExpressionWeeder(),
      FileNameWeeder(),
      UnicodeCharacterWeeder()
    )
  }

  def weed(tree: ParseTree, metaData: ParseMetaData) {
    var levelDerivs = mutable.MutableList.empty[String]
    var currentLevel = 0
    val weeders = getWeeders

    val queue: mutable.Queue[(ParseTreeNode, Int)] = mutable.Queue((tree.root, currentLevel))
    while (!queue.isEmpty) {
      val (node, level) = queue.dequeue()

      if (currentLevel < level) {
        levelDerivs = mutable.MutableList.empty[String]
        currentLevel = level
      }

      weeders.foreach(_.check(node, metaData))

      node match {
        case TreeNode(symbol, children) => {
          children.foreach(child => queue.enqueue((child, level + 1)))
        }
        case LeafNode(symbol) =>
      }
    }
  }
}
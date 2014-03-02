package joos.semantic

import joos.ast.declarations.{PackageDeclaration, TypeDeclaration}
import joos.ast.expressions.{SimpleNameExpression, QualifiedNameExpression, NameExpression}
import scala.collection.mutable

class NamespaceTrie {

  sealed abstract class TrieNode {
    def addPackage(segment: String): PackageNode

    def addType(typeDeclaration: TypeDeclaration): TypeNode
  }

  case class PackageNode(children: mutable.HashMap[String, TrieNode]) extends TrieNode {
    override def addPackage(segment: String) = {
      children.get(segment) match {
        case None => {
          val newNode = PackageNode(mutable.HashMap.empty)
          children.put(segment, newNode)
          newNode
        }
        case Some(node@PackageNode(children)) => node
        case Some(TypeNode(typeDeclaration)) => throw new NamespaceCollisionException(typeDeclaration.name)
      }
    }

    override def addType(typeDeclaration: TypeDeclaration) = {
      val typeName = typeDeclaration.name.standardName
      children.get(typeName) match {
        case None => {
          val newNode = TypeNode(typeDeclaration)
          children.put(typeName, newNode)
          newNode
        }
        case Some(e @ TypeNode(`typeDeclaration`)) => e
        case Some(otherNode) => throw new NamespaceCollisionException(typeDeclaration.name)
      }
    }
  }

  case class TypeNode(typeDeclaration: TypeDeclaration) extends TrieNode {
    override def addPackage(segment: String) = throw new NamespaceCollisionException(typeDeclaration.name)

    override def addType(typeDeclaration: TypeDeclaration) = throw new NamespaceCollisionException(typeDeclaration.name)
  }

  val root = PackageNode(mutable.HashMap.empty)

  def add(packageName: NameExpression, typeDeclaration: Option[TypeDeclaration]) {
    var node = root
    packageName.standardName.split('.') foreach {
      segment =>
        node = node.addPackage(segment)
    }

    typeDeclaration map (node.addType(_))
  }

  def getAllTypesInPackage(packageName: NameExpression): Seq[TypeDeclaration] = {
    var node: Option[TrieNode] = Some(root)
    packageName.standardName.split('.').foreach {
      segment =>
        node match {
          case Some(PackageNode(children)) => node = children.get(segment)
          case _ => throw new MissingTypeException(packageName)
        }
    }

    node match {
      case Some(PackageNode(children)) => children.values.collect { case TypeNode(typeDeclaration) => typeDeclaration }.toSeq
      case _ => throw new MissingTypeException(packageName)
    }
  }

  def getSimpleType(name: SimpleNameExpression): Option[TypeDeclaration] = {
    var types = Seq.empty[TypeDeclaration]
    val bfs = mutable.Queue[TrieNode](root)
    while (!bfs.isEmpty) {
      bfs.dequeue() match {
        case TypeNode(typeDeclaration) => if (typeDeclaration.name equals name) types = typeDeclaration +: types
        case PackageNode(children) => children.values foreach(bfs.enqueue(_))
      }
    }
    if (types.size > 1) throw new NamespaceCollisionException(name) else types.headOption
  }

  def getQualifiedType(name: QualifiedNameExpression): Option[TypeDeclaration] = {
    var node: Option[TrieNode] = Some(root)
    name.standardName.split('.').foreach {
      segment =>
        node match {
          case Some(PackageNode(children)) => node = children.get(segment)
          case _ => return None
        }
    }

    node match {
      case Some(TypeNode(typeDeclaration)) => return Some(typeDeclaration)
      case _ => return None
    }
  }
}

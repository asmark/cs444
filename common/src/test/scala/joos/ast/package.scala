package joos

import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{TreeNode, LeafNode}
import joos.syntax.tokens.{TokenKind, TerminalToken}

package object astspec {

  val nameToSimpleName = ProductionRule("Name", IndexedSeq("SimpleName"))
  val nameToQualifiedName = ProductionRule("Name", IndexedSeq("QualifiedName"))
  val qualifiedNameToName = ProductionRule("QualifiedName", IndexedSeq("Name", ".", "SimpleName"))

  val simpleNameTree = LeafNode(TerminalToken("someId", TokenKind.Id))
  val qualifiedNameTree = TreeNode(
    qualifiedNameToName, null,
    IndexedSeq(
      TreeNode(nameToSimpleName, null, IndexedSeq(simpleNameTree)),
      LeafNode(TerminalToken(".", TokenKind.Dot)), simpleNameTree))

  val nameToQualifiedNameTree = TreeNode(nameToQualifiedName, null, IndexedSeq(qualifiedNameTree))
  val nameToSimpleNameTree = TreeNode(nameToSimpleName, null, IndexedSeq(simpleNameTree))

}

package joos

import joos.language.ProductionRule
import joos.parsetree.{TreeNode, LeafNode}
import joos.tokens.{TokenKind, TerminalToken}

package object ast {

  private object ProductionRules {
    val nameToSimpleName = ProductionRule("Name", IndexedSeq("SimpleName"))
    val nameToQualifiedName = ProductionRule("Name", IndexedSeq("QualifiedName"))
    val qualifiedNameToName = ProductionRule("QualifiedName", IndexedSeq("Name", ".", "SimpleName"))
  }

  object AstSpecHelper {
    import ProductionRules._
    val simpleNameTree = LeafNode(TerminalToken("someId", TokenKind.Id))
    val qualifiedNameTree = TreeNode(qualifiedNameToName, null,
      IndexedSeq(TreeNode(nameToSimpleName, null, IndexedSeq(simpleNameTree)),
        LeafNode(TerminalToken(".", TokenKind.Dot)), simpleNameTree))

    val nameToQualifiedNameTree = TreeNode(nameToQualifiedName, null, IndexedSeq(qualifiedNameTree))
    val nameToSimpleNameTree = TreeNode(nameToSimpleName, null, IndexedSeq(simpleNameTree))
  }

}

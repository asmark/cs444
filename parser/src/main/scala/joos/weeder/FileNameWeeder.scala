package joos.weeder

import joos.parser.ParseMetaData
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.tokens.NonTerminalToken

case class FileNameWeeder() extends Weeder {

  private final def ErrorMessage(actualFileName: String, expectedFileName: String) = s"File name (${actualFileName}}) must match Class/Interface declaration name (${expectedFileName})"
  private final val ClassNameIndex = 2

  def check(ptn: ParseTreeNode, md: ParseMetaData) {
    ptn match {
      case TreeNode(_,NonTerminalToken(InterfaceDeclaration, InterfaceDeclaration), children) => checkFileName(children(ClassNameIndex), md)
      case TreeNode(_,NonTerminalToken(ClassDeclaration, ClassDeclaration), children) => checkFileName(children(ClassNameIndex), md)
      case _ =>
    }
  }

  private def checkFileName(identifierNode: ParseTreeNode, md: ParseMetaData) {
    val actualFileName = identifierNode.token.lexeme
    val expectedFileName = md.fileName
    if (!(actualFileName equals expectedFileName)) throw new WeederException(ErrorMessage(actualFileName, expectedFileName))
  }
}

package joos.ast.declarations

import joos.ast.exceptions.AstConstructionException
import joos.ast.expressions.NameExpression
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.semantic.{BlockEnvironment, ModuleEnvironment, TypeEnvironment}

case class ImportDeclaration(name: NameExpression, isOnDemand: Boolean) extends Declaration

object ImportDeclaration {
   def apply(ptn: ParseTreeNode)(implicit moduleEnvironment: ModuleEnvironment): Seq[ImportDeclaration] = {
     implicit val typeEnvironment = new TypeEnvironment
     implicit val blockEnvironment = BlockEnvironment(None) // implicit for NameExpression
     ptn match {
       case TreeNode(ProductionRule("ImportDeclarations", Seq("ImportDeclaration")), _, children) =>
         return ImportDeclaration(children(0))
       case TreeNode(ProductionRule("ImportDeclarations", Seq("ImportDeclarations", "ImportDeclaration")),_,children) =>
         return ImportDeclaration(children(0)) ++ ImportDeclaration(children(1))
       case TreeNode(ProductionRule("ImportDeclaration", _), _, children) =>
         return ImportDeclaration(children(0))
       case TreeNode(ProductionRule("SingleTypeImportDeclaration", _), _, children) =>
         return Seq(ImportDeclaration(NameExpression(children(1)), false))
       case TreeNode(ProductionRule("TypeImportOnDemandDeclaration", _), _, children) =>
         return Seq(ImportDeclaration(NameExpression(children(1)), true))
       case _ => throw new AstConstructionException("No valid production rule to create ImportDeclaration")
     }
   }
 }

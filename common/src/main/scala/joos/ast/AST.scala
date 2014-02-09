package joos.ast

import joos.tokens.TokenKind.TokenKindValue

class AST {

  class ASTNode {

  }

  class CompilationUnit(val packageDeclaration: Option[PackageDeclaration],
                        val importDeclarations: Option[ImportDeclaration],
                        val typeDeclaration: Option[TypeDeclaration]) extends ASTNode {

  }

  class PackageDeclaration(val packageName: Name) extends ASTNode {

  }

  trait ImportDeclaration extends ASTNode {

  }

  class SingleTypeImportDeclaration(val name: Name) extends ImportDeclaration {

  }

  class TypeImportOnDemandDeclaration(val name: Name) extends ImportDeclaration {

  }

  trait TypeDeclaration extends ASTNode {

  }

  class ClassDeclaration(val modifiers: Seq[String],
                         val identifier: String,
                         val superClassType: Option[ClassOrInterfaceType], // SuperClause: extends ClassType
                         val interfaces: Option[Seq[ClassOrInterfaceType]], // Interfaces: implements InterfaceTypeList
                         val classBody: Option[Seq[ClassBodyDeclaration]]) extends TypeDeclaration {

  }

  abstract class ClassBodyDeclaration extends ASTNode {

  }

  class FieldDeclaration(val modifiers: Seq[String],
                         val typeNonTerminal: TypeNonTerminal,
                         val variableDeclarator: VariableDeclarator) extends ClassBodyDeclaration {

  }

   // Type
  abstract class TypeNonTerminal extends ASTNode {

  }

  // Consolidated NumericType and boolean
  class PrimitiveType(val kind: TokenKindValue) extends TypeNonTerminal with ArrayType {

  }

  //TODO: Comparess it?
  trait ReferenceType extends TypeNonTerminal {

  }

  trait ClassOrInterfaceType extends ReferenceType {

  }

  trait ArrayType extends ReferenceType {

  }

  class VariableDeclarator(val variableDeclaratorId: String,
                           val variableInitializer: Option[Expression]) extends ASTNode {

  }

  abstract class Expression extends DimExpr {

  }

  class ConditionalOrExpression(val conditionalOrExpression: Option[ConditionalOrExpression],
                                val conditionalAndExpression: Option[ConditionalAndExpression])
                                extends Expression with AssignmentExpression {

  }

  class ConditionalAndExpression(val conditionalAndExpression: Option[ConditionalAndExpression],
                                 val inclusiveOrExpression: InclusiveOrExpression) extends ASTNode {

  }

  class InclusiveOrExpression(val inclusiveOrExpression: Option[InclusiveOrExpression],
                              val exclusiveOrExpression: ExclusiveOrExpression) extends ASTNode {

  }

  class ExclusiveOrExpression(val exclusiveOrExpression: Option[ExclusiveOrExpression],
                              val AndExpression: AndExpression) extends ASTNode {

  }

  class AndExpression(val andExpression: AndExpression, val EqualityExpression: EqualityExpression) extends ASTNode {

  }

  abstract class EqualityExpression (val relationalExpression: RelationalExpression) extends ASTNode {

  }

  class EqualExpression(val equalityExpression: EqualityExpression,
                        override val relationalExpression: RelationalExpression)
                        extends EqualityExpression(relationalExpression) {

  }

  class NotEqualExpression(val equalityExpression: EqualityExpression,
                           override val relationalExpression: RelationalExpression)
                           extends EqualityExpression(relationalExpression) {

  }

  trait RelationalExpression extends ASTNode {

  }

  class RelationalExpressionWithOperator(val relationalExpression: RelationalExpression,
                                         val additiveExpression: AdditiveExpression,
                                         val relationalOperator: TokenKindValue) extends RelationalExpression{

  }

  class RelationalExpressionWithReferenceType(val relationalExpression: RelationalExpression,
                                              val referenceType: ReferenceType) extends RelationalExpression {

  }

  class AdditiveExpression(val additiveExpression: Option[AdditiveExpression],
                           val additiveOperator: Option[TokenKindValue],
                           val multiplicativeExpression: MultiplicativeExpression) extends RelationalExpression {

  }

  class MultiplicativeExpression(val unaryExpression: UnaryExpression,
                                 val operator: Option[TokenKindValue],
                                 val multiplicativeExpression: Option[MultiplicativeExpression]) extends ASTNode {
    
  }

  trait UnaryExpression extends ASTNode {

  }

  class NegativeUnaryExpression(val unaryExpression: UnaryExpression) extends UnaryExpression {

  }

  trait UnaryExpressionNotPlusMinus extends UnaryExpression {

  }

  trait PostfixExpression extends UnaryExpressionNotPlusMinus {

  }

  trait Primary extends PostfixExpression {

  }

  trait PrimaryNoNewArray extends Primary {

  }

  trait ArrayCreationExpression extends Primary {

  }

  class ArrayCreationExpressionWithPrimitiveType(val primitiveType: PrimitiveType,
                                                 val dimExpr: DimExpr) extends ArrayCreationExpression {

  }

  class ArrayCreationExpressionWithClassOrInterfaceType(val classOrInterfaceType: ClassOrInterfaceType,
                                                        val dimExpr: DimExpr) extends ArrayCreationExpression

  trait DimExpr extends ASTNode {

  }

  trait Literal extends PrimaryNoNewArray {

  }

  class DecimalIntLiteral extends Literal {

  }

  class BooleanLiteral(val kind: TokenKindValue) extends Literal {

  }

  class CharacterLiteral extends Literal {

  }

  class StringLiteral extends Literal {

  }

  class Null extends Literal {

  }

  class This extends PrimaryNoNewArray {

  }

  // TODO: expression extends primary?
  class BracketedExpression(val expression: Expression) extends PrimaryNoNewArray {

  }

  class ClassInstanceCreationExpression(val classType: ClassOrInterfaceType,
                                        val argumentList: Option[Seq[Expression]])
    extends PrimaryNoNewArray with StatementExpression {

  }

  class FieldAccess(val primary: Primary, val identifier: String) extends PrimaryNoNewArray with LeftHandSide {

  }

  abstract class MethodInvocation(val argumentList: Option[Seq[Expression]])
    extends PrimaryNoNewArray with Statement {

  }

  class MethodInvocationByName(val name: Name, override val argumentList: Option[Seq[Expression]])
    extends MethodInvocation(argumentList) {

  }

  class MethodInvocationByPrimaryIdentifier(val primary: Primary,
                                            val identifier: String,
                                            override val argumentList: Option[Seq[Expression]])
                                            extends MethodInvocation(argumentList) {

  }

  abstract class ArrayAccess(val expression: Expression) extends PrimaryNoNewArray with LeftHandSide {

  }

  class ArrayAccessByName(val name: Name, override val expression: Expression) extends ArrayAccess(expression) {

  }

  class ArrayAccessByPrimaryNoNewArray(val primary: PrimaryNoNewArray, override val expression: Expression)
    extends ArrayAccess(expression) {

  }

  class NegatedUnaryExpression(val unaryExpression: UnaryExpression) extends UnaryExpressionNotPlusMinus {

  }

  abstract class CastExpression extends UnaryExpression {

  }

  class CastExpressionByPrimitiveType(val primitiveType: PrimitiveType,
                                      val unaryExpression: UnaryExpression,
                                      val dims: Option[Dims]) extends CastExpression {

  }

  class CastExpressionByExpression(val expression: Expression,
                                   val unaryExpressionNotPlusMinus: UnaryExpressionNotPlusMinus)
                                   extends CastExpression {

  }

  class CastExpressionByName(val name: Name,
                             val unaryExpressionNotPlusMinus: UnaryExpressionNotPlusMinus,
                             val dims: Dims)
                             extends CastExpression {

  }

  class Dims extends DimExpr{

  }

  class Assignment(val leftHandSide: LeftHandSide, val assignmentExpression: AssignmentExpression)
    extends Expression with AssignmentExpression with StatementExpression {

  }

  trait LeftHandSide extends ASTNode {

  }

  trait AssignmentExpression extends ASTNode {

  }

  class MethodDeclaration(val header: MethodHeader, val body: MethodBody) extends ClassBodyDeclaration {

  }

  class MethodHeader(val modifiers: Seq[String],
                     val theType: Option[TypeNonTerminal],
                     val declaration: MethodDeclaration) extends ASTNode {

  }

  trait MethodBody extends ASTNode {

  }

  // ;
  class EmptyStatement extends MethodBody with Statement with StatementNoShortIf with TypeDeclaration {

  }

  class Block(val statements: Option[Seq[BlockStatement]]) extends MethodBody with Statement with StatementNoShortIf {

  }

  trait BlockStatement extends ASTNode {

  }

  class LocalVariableDeclaration(val theType: TypeNonTerminal, val variableDeclarator: VariableDeclarator)
    extends BlockStatement with ForInit {

  }

  trait Statement extends BlockStatement {

  }

  trait StatementExpression extends Statement with StatementNoShortIf with ForInit with ForUpdate {

  }

  // TODO: Not used?
  class PreIncrementExpression extends StatementExpression {

  }

  // TODO: Not used?
  class PreDecrementExpression extends StatementExpression {

  }

  // TODO: Not used?
  class PostIncrementExpression extends StatementExpression {

  }

  // TODO: Not used?
  class PostDecrementExpression extends StatementExpression {

  }

  class ReturnStatement(val expression: Option[Expression]) extends Statement with StatementNoShortIf {

  }

  class IfThenStatement(val expression: Expression, val statement: Statement) extends Statement {

  }

  class IfThenElseStatement(val expression: Expression,
                            val statementNoShortIf: StatementNoShortIf,
                            val statement: Statement) extends Statement {

  }

  trait StatementNoShortIf extends ASTNode {

  }

  class IfThenElseStatementNoShortIf(val expression: Expression,
                                     val statementForTrue: StatementNoShortIf,
                                     val statementForFalse: StatementNoShortIf) extends StatementNoShortIf {

  }

  class WhileStatementNoShortIf(val statement: Statement, val statementNoShortIf: StatementNoShortIf)
    extends StatementNoShortIf {

  }

  class ForStatementNoShortIf(val forInit: Option[ForInit],
                              val expression: Option[Expression],
                              val forUpdate: ForUpdate,
                              val statementNoShortIf: StatementNoShortIf) extends StatementNoShortIf {

  }

  trait ForInit extends ASTNode {

  }

  trait ForUpdate extends ASTNode {

  }

  class WhileStatement(val expression: Expression, val statement: Statement) extends Statement {

  }

  class ForStatement(val forInit: Option[ForInit],
                     val expression: Option[Expression],
                     val forUpdate: ForUpdate,
                     val statementNoShortIf: StatementNoShortIf) extends Statement {

  }

  class ConstructorDeclaration(val modifiers: Seq[String],
                               val declarator: ConstructorDeclarator,
                               val constructorBody: ConstructorBody) extends ClassBodyDeclaration {

  }

  class ConstructorDeclarator(val simpleName: String, val formalParameters: Option[Seq[FormalParameter]])
    extends ASTNode {

  }

  class FormalParameter(val theType: TypeNonTerminal, val variableDeclaratorId: String) extends ASTNode {

  }

  class ConstructorBody(val blockStatement: Option[Seq[BlockStatement]]) extends ASTNode {

  }

  abstract class Name extends ClassOrInterfaceType with PostfixExpression with LeftHandSide with ArrayType {

  }

  class SimpleName(val identifier: String) extends Name {

  }

  class QualifiedName(val name: Name, val identifier: String) extends Name {

  }

  class InterfaceDeclaration(val modifiers: Seq[String],
                             val identifier: String,
                             val extendsInterfaces: Option[Seq[ClassOrInterfaceType]],
                             val body: InterfaceBody) extends TypeDeclaration {

  }

  class InterfaceBody(val interfaceMemberDeclarations: Option[InterfaceMemberDeclarations]) extends ASTNode {

  }

  class InterfaceMemberDeclarations(val interfaceMemberDeclarations: Option[InterfaceMemberDeclarations],
                                    val abstractMethodDeclaration: AbstractMethodDeclaration) extends ASTNode {

  }

  class AbstractMethodDeclaration(val header: MethodHeader) extends ASTNode {

  }
}
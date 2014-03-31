package joos.codegen.generators

import joos.ast.CompilationUnit
import joos.ast.declarations._
import joos.ast.expressions._
import joos.ast.statements._
import joos.codegen.AssemblyFileManager
import joos.codegen.assembler._
import joos.semantic.types.AstEnvironmentVisitor
import scala.language.implicitConversions

class CodeGenerationVisitor(implicit val unit: CompilationUnit, val assemblyManager: AssemblyFileManager) extends AstEnvironmentVisitor {
  override def apply(field: FieldDeclaration) {
    super.apply(field)
    field.generateAssembly()
  }

  override def apply(importDeclaration: ImportDeclaration) {
    super.apply(importDeclaration)
    importDeclaration.generateAssembly()
  }

  override def apply(methodDeclaration: MethodDeclaration) {
    methodDeclaration.generateAssembly()
    super.apply(methodDeclaration)
  }

  override def apply(packaged: PackageDeclaration) {
    super.apply(packaged)
    packaged.generateAssembly()
  }

  override def apply(variable: SingleVariableDeclaration) {
    super.apply(variable)
    variable.generateAssembly()
  }

  override def apply(typed: TypeDeclaration) {
    super.apply(typed)
    typed.generateAssembly()
  }

  override def apply(expression: ArrayAccessExpression) {
    super.apply(expression)
    expression.generateAssembly()
  }

  override def apply(expression: ArrayCreationExpression) {
    super.apply(expression)
    expression.generateAssembly()
  }

  override def apply(expression: AssignmentExpression) {
    super.apply(expression)
    expression.generateAssembly()
  }

  override def apply(literal: BooleanLiteral) {
    literal.generateAssembly()
  }

  override def apply(expression: CastExpression) {
    super.apply(expression)
    expression.generateAssembly()
  }

  override def apply(literal: CharacterLiteral) {
    literal.generateAssembly()
  }

  override def apply(expression: ClassInstanceCreationExpression) {
    super.apply(expression)
    expression.generateAssembly()
  }

  override def apply(expression: FieldAccessExpression) {
    super.apply(expression)
    expression.generateAssembly()
  }

  override def apply(expression: InfixExpression) {
    super.apply(expression)
    expression.generateAssembly()
  }

  override def apply(expression: InstanceOfExpression) {
    super.apply(expression)
    expression.generateAssembly()
  }

  override def apply(literal: IntegerLiteral) {
    literal.generateAssembly()
  }

  override def apply(expression: MethodInvocationExpression) {
    super.apply(expression)
    expression.generateAssembly()
  }

//  override def apply(expression: NullLiteral) {
//
//  }

  override def apply(expression: ParenthesizedExpression) {
    super.apply(expression)
  }

  override def apply(expression: PrefixExpression) {
    super.apply(expression)
    expression.generateAssembly()
  }

  override def apply(expression: QualifiedNameExpression) {
    super.apply(expression)
    expression.generateAssembly()
  }

  override def apply(expression: SimpleNameExpression) {
    super.apply(expression)
    expression.generateAssembly()
  }

  override def apply(literal: StringLiteral) {
    literal.generateAssembly()
  }

  override def apply(expression: ThisExpression) {
    super.apply(expression)
    expression.generateAssembly()
  }

  override def apply(expression: VariableDeclarationExpression) {
    super.apply(expression)
    expression.generateAssembly()
  }

  override def apply(expression: VariableDeclarationFragment) {
    super.apply(expression)
    expression.generateAssembly()
  }

  override def apply(statement: ExpressionStatement) {
    super.apply(statement)
    statement.generateAssembly()
  }

  override def apply(statement: ForStatement) {
    super.apply(statement)
    statement.generateAssembly()
  }

  override def apply(statement: IfStatement) {
    super.apply(statement)
    statement.generateAssembly()
  }

  override def apply(statement: ReturnStatement) {
    super.apply(statement)
    statement.generateAssembly()
  }

  override def apply(statement: WhileStatement) {
    super.apply(statement)
    statement.generateAssembly()
  }

  override def apply(unit: CompilationUnit) {
    super.apply(unit)
    unit.generateAssembly()
  }

  override def apply(block: Block) {
    super.apply(block)
    block.generateAssembly()
  }

  implicit def toFieldAssembler(field: FieldDeclaration): FieldDeclarationAssembler = new FieldDeclarationAssembler(field)

  implicit def toImportDeclarationAssembler(importDeclaration: ImportDeclaration): ImportDeclarationAssembler =
    new ImportDeclarationAssembler(importDeclaration)

  implicit def toMethodDeclarationAssembler(methodDeclaration: MethodDeclaration): MethodDeclarationAssembler =
    new MethodDeclarationAssembler(methodDeclaration)

  implicit def toPackageDeclarationAssembler(packageDelcaration: PackageDeclaration): PackageDeclarationAssembler =
    new PackageDeclarationAssembler(packageDelcaration)

  implicit def toSingleVarDeclarationAssembler(variable: SingleVariableDeclaration): SingleVarDeclarationAssembler =
    new SingleVarDeclarationAssembler(variable)

  implicit def toTypeDeclarationAssembler(typed: TypeDeclaration): TypeDeclarationAssembler =
    new TypeDeclarationAssembler(typed)

  implicit def toArrayAccessExpressionAssembler(expression: ArrayAccessExpression): ArrayAccessExpressionAssembler =
    new ArrayAccessExpressionAssembler(expression)

  implicit def toArrayCreationExpressionAssembler(expression: ArrayCreationExpression): ArrayCreationExpressionAssembler =
    new ArrayCreationExpressionAssembler(expression)

  implicit def toAssignmentExpressionAssembler(expression: AssignmentExpression): AssignmentExpressionAssembler =
    new AssignmentExpressionAssembler(expression)

  implicit def toCastExpressionAssembler(expression: CastExpression): CastExpressionAssembler =
    new CastExpressionAssembler(expression)

  implicit def toClassInstanceCreationExpressionAssembler(expression: ClassInstanceCreationExpression): ClassInstanceCreationExpressionAssembler =
    new ClassInstanceCreationExpressionAssembler(expression)

  implicit def toFieldAccessExpressionAssembler(expression: FieldAccessExpression): FieldAccessExpressionAssembler =
    new FieldAccessExpressionAssembler(expression)

  implicit def toInfixExpressionAssembler(expression: InfixExpression): InfixExpressionAssembler =
    new InfixExpressionAssembler(expression)

  implicit def toInstanceOfExpressionAssembler(expression: InstanceOfExpression): InstanceOfExpressionAssembler =
    new InstanceOfExpressionAssembler(expression)

  implicit def toMethodInvocationExpressionAssembler(expression: MethodInvocationExpression): MethodInvocationExpressionAssembler =
    new MethodInvocationExpressionAssembler(expression)

  implicit def toPrefixExpressionAssembler(expression: PrefixExpression): PrefixExpressionAssembler =
    new PrefixExpressionAssembler(expression)

  implicit def toQualifiedNameExpressionAssembler(expression: QualifiedNameExpression): QualifiedNameExpressionAssembler =
    new QualifiedNameExpressionAssembler(expression)

  implicit def toSimpleNameExpressionAssembler(expression: SimpleNameExpression): SimpleNameExpressionAssembler =
    new SimpleNameExpressionAssembler(expression)

  implicit def toThisExpressionAssembler(expression: ThisExpression): ThisExpressionAssembler =
    new ThisExpressionAssembler(expression)

  implicit def toVariableDeclarationExpressionAssembler(expression: VariableDeclarationExpression): VariableDeclarationExpressionAssembler =
    new VariableDeclarationExpressionAssembler(expression)

  implicit def toVariableDeclarationFragmentAssembler(expression: VariableDeclarationFragment): VariableDeclarationFragmentAssembler =
    new VariableDeclarationFragmentAssembler(expression)

  implicit def toExpressionStatementAssember(statement: ExpressionStatement): ExpressionStatementAssembler =
    new ExpressionStatementAssembler(statement)

  implicit def toForStatementAssember(statement: ForStatement): ForStatementAssembler =
    new ForStatementAssembler(statement)

  implicit def toIfStatementAssember(statement: IfStatement): IfStatementAssembler =
    new IfStatementAssembler(statement)

  implicit def toReturnStatementAssember(statement: ReturnStatement): ReturnStatementAssembler =
    new ReturnStatementAssembler(statement)

  implicit def toWhileStatementAssember(statement: WhileStatement): WhileStatementAssembler =
    new WhileStatementAssembler(statement)

  implicit def toCompilationUnitAssembler(unit: CompilationUnit): CompilationUnitAssembler = new CompilationUnitAssembler(unit)

  implicit def toBlockAssembler(block: Block): BlockAssembler = new BlockAssembler(block)

  implicit def toBooleanLiteralAssembler(literal: BooleanLiteral): BooleanLiteralAssembler = new BooleanLiteralAssembler(literal)

  implicit def toIntegerLiteralAssembler(literal: IntegerLiteral): IntegerLiteralAssembler = new IntegerLiteralAssembler(literal)

  implicit def toStringLiteralAssembler(literal: StringLiteral): StringLiteralAssembler = new StringLiteralAssembler(literal)

  implicit def toCharacterLiteralAssembler(literal: CharacterLiteral): CharacterLiteralAssembler = new CharacterLiteralAssembler(literal)
}

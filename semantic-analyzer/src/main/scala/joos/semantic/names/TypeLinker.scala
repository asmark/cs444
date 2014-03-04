package joos.semantic.names

import joos.ast._
import joos.ast.declarations._
import joos.ast.expressions.{NameExpression, VariableDeclarationExpression}
import joos.semantic.BlockEnvironment

/**
 * TypeLinker is responsible for the following name resolution checks:
 * No single-type-import declaration clashes with the class or interface declared in the same file.
 *
 * No two single-type-import declarations clash with each other.
 *
 * All type names must resolve to some class or interface declared in some file listed on the Joos command line.
 *
 * All simple type names must resolve to a unique class or interface.
 *
 * When a fully qualified name resolves to a type, no strict prefix of the fully qualified name can resolve to a type in the same environment.
 *
 * No package names or prefixes of package names of declared packages, single-type-import declarations or import-on-demand declarations that are
 * used may resolve to types, except for types in the default package.
 *
 * Every import-on-demand declaration must refer to a package declared in some file listed on the Joos command line. That is,
 * the import-on-demand declaration must refer to a package whose name appears as the package declaration in some source file,
 * or whose name is a prefix of the name appearing in some package declaration.
 */
class TypeLinker(implicit module: ModuleDeclaration) extends AstVisitor {
  private[this] implicit var typed: TypeDeclaration = null
  private[this] var unit: CompilationUnit = null
  private[this] var packaged: PackageDeclaration = null
  private[this] var method: MethodDeclaration = null
  private[this] var block: BlockEnvironment = null

  override def apply(unit: CompilationUnit) {
    this.unit = unit
    packaged = unit.packageDeclaration
    unit.add(ImportDeclaration(NameExpression("java.lang"), true))
    unit.addSelfPackage()
    unit.importDeclarations foreach (unit.add(_))
    unit.typeDeclaration.map(_.accept(this))
  }

  override def apply(typed: TypeDeclaration) {
    this.typed = typed
    typed.compilationUnit = unit
    typed.packageDeclaration = packaged
    typed.methods.foreach(_.accept(this))
  }

  override def apply(method: MethodDeclaration) {
    this.method = method
//    method.compilationUnit = unit
    method.environment = method.parameters.foldRight(BlockEnvironment()) {
      (variable, environment) => environment.add(variable)
    }
    method.typeDeclaration = typed
    block = method.environment
    method.body.map(_.accept(this))
  }

  override def apply(block: Block) {
    val oldEnvironment = this.block
    block.statements.foreach(_.accept(this))
    this.block = oldEnvironment
  }

  override def apply(statement: IfStatement) {
    statement.condition.accept(this)
    val oldBlock = block
    statement.trueStatement.accept(this)
    block = oldBlock
    statement.falseStatement.map(_.accept(this))
    block = oldBlock
  }

  override def apply(statement: ExpressionStatement) {
    statement.expression.accept(this)
  }

  override def apply(statement: WhileStatement) {
    val oldBlock = block
    statement.condition.accept(this)
    statement.body.accept(this)
    block = oldBlock
  }

  override def apply(expression: VariableDeclarationExpression) {
    block = block.add(expression)
  }

  override def apply(statement: ForStatement) {
    val oldBlock = block
    statement.initialization.map(_.accept(this))
    statement.condition.map(_.accept(this))
    statement.update.map(_.accept(this))
    statement.body.accept(this)
    block = oldBlock
  }
}

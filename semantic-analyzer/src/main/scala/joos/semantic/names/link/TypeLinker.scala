package joos.semantic.names.link

import joos.ast._
import joos.ast.declarations._
import joos.ast.expressions._
import joos.ast.visitor.AstCompleteVisitor
import joos.semantic.MissingTypeException
import joos.ast.types.{SimpleType, Type, PrimitiveType, ArrayType}
import joos.ast.types.PrimitiveType.PrimitiveType

/**
 * TypeLinker is responsible for the following name resolution checks:
 *
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
class TypeLinker(implicit module: ModuleDeclaration, unit: CompilationUnit) extends AstCompleteVisitor {

  def resolveType(typed: Type) {
    typed match {
      case _: PrimitiveType =>
      case ArrayType(typed, dimensions) => resolveType(typed)
      case SimpleType(name) => resolveType(name)
    }
  }

  def resolveType(name: NameExpression) {
    val typeDeclaration = unit.getVisibleType(name)
    if (typeDeclaration.isEmpty) {
      throw new MissingTypeException(name)
    }
  }

  override def apply(unit: CompilationUnit) {
    unit.addSelfPackage()

    super.apply(unit)
  }

  override def apply(typed: TypeDeclaration) {
    typed.superType foreach resolveType
    typed.superInterfaces foreach resolveType

    super.apply(typed)
  }

  override def apply(field: FieldDeclaration) {
    resolveType(field.declarationType)

    super.apply(field)
  }

  override def apply(method: MethodDeclaration) {
    method.returnType foreach resolveType
    method.parameters foreach (parameter => resolveType(parameter.declarationType))

    super.apply(method)
  }

  override def apply(variable: SingleVariableDeclaration) {
    resolveType(variable.variableType)

    super.apply(variable)
  }

  override def apply(expression: ArrayCreationExpression) {
    resolveType(expression.arrayType)

    super.apply(expression)
  }

  override def apply(expression: ClassInstanceCreationExpression) {
    resolveType(expression.classType)

    super.apply(expression)
  }

  override def apply(expression: InstanceOfExpression) {
    resolveType(expression.classType)

    super.apply(expression)
  }

  override def apply(expression: CastExpression) {
    resolveType(expression.castType)

    super.apply(expression)
  }

  override def apply(expression: VariableDeclarationExpression) {
    resolveType(expression.variableType)

    super.apply(expression)
  }
}

package joos.semantic.names

import joos.ast._
import joos.ast.declarations._
import joos.ast.expressions.NameExpression

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
class TypeLinker(implicit module: ModuleDeclaration) extends AstVisitor {
  override def apply(unit: CompilationUnit) {
    unit.add(ImportDeclaration(NameExpression("java.lang"), true))
    unit.addSelfPackage()
    unit.importDeclarations foreach (unit.add(_))
  }

  // TODO
  // All simple type names must resolve to a unique class or interface.

  // TODO
  // No package names or prefixes of package names of declared packages, single-type-import declarations or import-on-demand declarations that are
  // used may resolve to types, except for types in the default package.
}

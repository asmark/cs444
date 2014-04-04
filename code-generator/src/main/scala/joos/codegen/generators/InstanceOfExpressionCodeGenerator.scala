package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.ast.expressions.InstanceOfExpression
import joos.ast.types.{PrimitiveType, ArrayType, SimpleType}
import joos.semantic._

class InstanceOfExpressionCodeGenerator(expression: InstanceOfExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {
    expression.expression.generate()


    expression.classType match {
      case dst: SimpleType => {
        expression.expression.expressionType match {
          case ArrayType(_,_) => {
            require(dst.declaration.fullName == javaLangObject.standardName ||
                dst.declaration.fullName == javaLangObject.standardName ||
                dst.declaration.fullName == javaIOSerializable.standardName)
            appendText(
              mov(Eax, 1) :#"Set result to true for array casting to Object, Cloneable or IOSerializable"
            )
          }
          case PrimitiveType.NullType => {
            appendText(
              mov(Eax, 0) :#"Set result to false for null casting to reference type"
            )
          }
          case src: SimpleType => {
            val dstTypeDeclration = environment.typeEnvironment.compilationUnit.getVisibleType(dst.name)
            require(dstTypeDeclration.isDefined)
            val dstTypeIdx = environment.staticDataManager.getTypeIndex(dstTypeDeclration.get)

            appendText(:#(s"[BEGIN] Casting ${src.standardName} to ${dst.standardName}"))
            prologue(0)
            appendText(
              #>,
              mov(Ebx, Eax),
              add(Ebx, toExpression(SelectorTableOffset)),
              mov(Ebx, at(Ebx)) :#"EBX should point to the sub type table",
              add(Ebx, dstTypeIdx),
              mov(Ebx, at(Ebx)) :#"Look up value in the subtype table",
              mov(Eax, Ebx) :#s"Set result to true/false for ${src.standardName} casting to ${dst.standardName}",
              #<
            )
            epilogue(0)
            appendText(:#(s"[END] Casting Casting ${src.standardName} to ${dst.standardName}"))
          }
          case _ => {}
        }
      }
      case ArrayType(_,_) => {

      }
    }
  }

}

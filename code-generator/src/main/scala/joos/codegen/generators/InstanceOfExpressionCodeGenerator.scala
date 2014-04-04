package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.expressions.InstanceOfExpression
import joos.ast.types._
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.core.Logger
import joos.semantic.types.TypeCheckingException

class InstanceOfExpressionCodeGenerator(instanceOf: InstanceOfExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  // InstaceOf := leftType instanceof rightType
  override def generate() {
    instanceOf.expression.expressionType match {

      case SimpleType(_) | ArrayType(_,_) => {
        instanceOf.classType match {
          case ArrayType(rightElementType, _) => {
            val subtypeIndex = rightElementType match {
              case rightSimpleType: SimpleType => {
                require(rightSimpleType.declaration != null)
                environment.staticDataManager.getArrayTypeIndex(rightSimpleType.declaration)
              }
              case rightPrimitiveType: PrimitiveType => {
                environment.staticDataManager.getArrayTypeIndex(rightPrimitiveType)
              }
            }

            generateObjectCheck(subtypeIndex)
          }
          case rightType: SimpleType => {
            // Castable to Object, Cloneable and Serializable.
            require(rightType.declaration != null)
            val subtypeIndex = environment.staticDataManager.getTypeIndex(rightType.declaration)
            generateObjectCheck(subtypeIndex)
          }
          case NullType => {
            // TODO: this should go into static type check
            throw new TypeCheckingException(
              "instanceof",
              s"${instanceOf.classType.standardName} are not related types ${instanceOf.expression.expressionType.standardName}")
          }
        }
      }

      case _ =>
        Logger.logWarning(s"No Support for ${instanceOf.expression.expressionType} in instanceof checks in ${instanceOf}")
    }
  }

  private def generateObjectCheck(subtypeIndex: Int) {
    appendText(
      :#(s"[BEGIN] InstanceOf check ${instanceOf}"),
      :#("Look up left hand side"),
      #>
    )
    instanceOf.expression.generate()
    val endLabel = nextLabel("is_null_object")
    appendText(
      #<,
      emptyLine,
      cmp(Eax, 0),
      je(endLabel),
      mov(Eax, at(Eax + SubtypeTableOffset)) :# "Put address of subtype table in eax",
      mov(Eax, at(Eax + 4 * subtypeIndex)) :# "Look up value in subtype table for instance check",
      endLabel::,
      :#(s"[END] InstanceOf check ${instanceOf}")
    )
  }
}

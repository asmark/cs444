package joos.codegen.generators

import joos.ast.expressions.CastExpression
import joos.ast.types._
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.core.Logger
import joos.assemgen._
import joos.assemgen.Register._

class CastExpressionCodeGenerator(castExpression: CastExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  // CastExpression := (leftType) rightType
  override def generate() {
    castExpression.expression.expressionType match {

      case SimpleType(_) | ArrayType(_,_) => {
        castExpression.castType match {
          case ArrayType(leftElementType, _) => {
            val subtypeIndex = leftElementType match {
              case leftSimpleType: SimpleType => {
                require(leftSimpleType.declaration != null)
                environment.staticDataManager.getArrayTypeIndex(leftSimpleType.declaration)
              }
              case leftPrimitiveType: PrimitiveType => {
                environment.staticDataManager.getArrayTypeIndex(leftPrimitiveType)
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
          case NullType => castExpression.expression.generate()
        }
      }

      case PrimitiveType.ShortType => {
        castExpression.castType match {
          case PrimitiveType.ByteType | PrimitiveType.CharType => {
            castExpression.expression.generate()
            castExpression.expression.generate()
            val negativeLabel = nextLabel("negative_short")
            val endLabel = nextLabel("end")
            appendText(:#("[BEGIN] Casting Short to Byte/Char"))
            prologue(0)
            appendText(
              mov(Ebx, toExpression(255)),
              cmp(Eax, 0),
              jl(labelReference(negativeLabel)),
              // Positive
              and(Eax, Ebx),
              jmp(endLabel),
              // Negative
              negativeLabel::,
              neg(Eax),
              and(Eax, Ebx),
              neg(Eax),
              endLabel::
            )
            epilogue(0)
            appendText(:#("[END] Casting Casting Short to Byte/Char"))
          }
          case _ => castExpression.expression.generate()
        }
      }
      case PrimitiveType.IntegerType => {
        castExpression.castType match {
          case PrimitiveType.ByteType | PrimitiveType.CharType => {
            castExpression.expression.generate()
            val negativeLabel = nextLabel("negative_int")
            val endLabel = nextLabel("end")
            appendText(:#("[BEGIN] Casting Int to Byte/Char"))
            prologue(0)
            appendText(
              mov(Ebx, toExpression(255)),
              cmp(Eax, 0),
              jl(labelReference(negativeLabel)),
              // Positive
              and(Eax, Ebx),
              jmp(endLabel),
              // Negative
              negativeLabel::,
              neg(Eax),
              and(Eax, Ebx),
              neg(Eax),
              endLabel::
            )
            epilogue(0)
            appendText(:#("[END] Casting Int to Byte/Char"))
          }
          case PrimitiveType.ShortType => {
            castExpression.expression.generate()
            val negativeLabel = nextLabel("negative_int")
            val endLabel = nextLabel("end")
            appendText(:#("[BEGIN] Casting Int to Short"))
            prologue(0)
            appendText(
              mov(Ebx, toExpression(65535)),
              cmp(Eax, 0),
              jl(labelReference(negativeLabel)),
              // Positive
              and(Eax, Ebx),
              jmp(endLabel),
              // Negative
              negativeLabel::,
              neg(Eax),
              and(Eax, Ebx),
              neg(Eax),
              endLabel::
            )
            epilogue(0)
            appendText(:#("[END] Casting Int to Short"))
          }
          case _ => castExpression.expression.generate()
        }
      }
      case PrimitiveType.ByteType | PrimitiveType.CharType | PrimitiveType.BooleanType =>
        castExpression.expression.generate()

      case _ =>
        Logger.logWarning(s"No Support for ${castExpression.expression.expressionType} in instanceof checks in ${castExpression}")
    }

  }

  private def generateObjectCheck(subtypeIndex: Int) {
    appendText(
      :#(s"[BEGIN] Cast check ${castExpression}"),
      :#("Look up left hand side"),
      #>
    )

    castExpression.expression.generate()

    val endLabel = nextLabel("is_null_object")
    appendText(
      #<,
      emptyLine,
      cmp(Eax, 0),
      je(endLabel),
      mov(Ebx, at(Eax + SubtypeTableOffset)) :# "Put address of subtype table in eax",
      mov(Ebx, at(Ebx + 4 * subtypeIndex)) :# "Look up value in subtype table for instance check",
      cmp(Ebx, 0) :# "Throw exception if it cannot be cast",
      je(exceptionLabel),
      endLabel::,
      :#(s"[END] Cast check ${castExpression}")
    )
  }
}
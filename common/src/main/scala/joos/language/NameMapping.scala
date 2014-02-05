package joos.language

object NameMapping {
  var symbols : Set[String] = Set()
  SymbolTokenMap map {case (key, value) => symbols += value}

  def getSymbols = symbols

  final val SymbolTokenMap = Map(
    "abstract" -> "Abstract",
    "default" -> "Default",
    "if" -> "If",
    "private" -> "Private",
    "this" -> "This",
    "boolean" -> "Boolean",
    "do" -> "Do",
    "implements" -> "Implements",
    "protected" -> "Protected",
    "throw" -> "Throw",
    "break" -> "Break",
    "double" -> "Double",
    "import" -> "Import",
    "public" -> "Public",
    "throws" -> "Throws",
    "byte" -> "Byte",
    "else" -> "Else",
    "instanceof" -> "InstanceOf",
    "return" -> "Return",
    "transient" -> "Transient",
    "case" -> "Case",
    "extends" -> "Extends",
    "int" -> "Int",
    "short" -> "Short",
    "try" -> "Try",
    "catch" -> "Catch",
    "final" -> "Final",
    "interface" -> "Interface",
    "static" -> "Static",
    "void" -> "Void",
    "char" -> "Char",
    "finally" -> "Finally",
    "long" -> "Long",
    "strictfp" -> "Strictfp",
    "volatile" -> "Volatile",
    "class" -> "Class",
    "float" -> "Float",
    "native" -> "Native",
    "super" -> "Super",
    "while" -> "While",
    "const" -> "Const",
    "for" -> "For",
    "new" -> "New",
    "switch" -> "Switch",
    "continue" -> "Continue",
    "goto" -> "Goto",
    "package" -> "Package",
    "synchronized" -> "Synchronized",

    "(" -> "LeftParen",
    ")" -> "RightParen",
    "{" -> "LeftBrace",
    "}" -> "RightBrace",
    "[" -> "LeftBracket",
    "]" -> "RightBracket",
    ":" -> "SemiColon",
    "," -> "Comma",
    "." -> "Dot",

    "=" -> "Assign",
    ">" -> "Greater",
    "<" -> "Less",
    "!" -> "Exclamation",
    "~" -> "Tilde",
    "?" -> "Question",
    ":" -> "Colon",
    "==" -> "Equal",
    "<=" -> "LessEqual",
    ">=" -> "GreaterEqual",
    "!=" -> "NotEqual",
    "&&" -> "And",
    "||" -> "Or",
    "++" -> "Increment",
    "--" -> "Decrement",
    "+" -> "Plus",
    "-" -> "Minus",
    "*" -> "Multiply",
    "/" -> "Divide",
    "&" -> "BitAnd",
    "|" -> "BitOr",
    "^" -> "Carrot",
    "%" -> "Modulo",
    "<<" -> "LeftShift",
    ">>" -> "RightShift",
    ">>>" -> "UnsignedShift",
    "+=" -> "PlusAssign",
    "-=" -> "MinusAssign",
    "*=" -> "MultiplyAssign",
    "/=" -> "DivideAssign",
    "&=" -> "BitAndAssign",
    "|=" -> "BitOrAssign",
    "^=" -> "CarrotAssign",
    "%=" -> "ModuloAssign",
    "<<=" -> "LeftShiftAssign",
    ">>=" -> "RightShiftAssign",
    ">>>=" -> "UnsignedRightShiftAssign"
  )
}

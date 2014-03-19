package joos.ast.compositions

import joos.semantic.BlockEnvironment

trait BlockLike {
  var blockEnvironment: BlockEnvironment = _
}

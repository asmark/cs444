package joos.ast.compositions

import joos.semantic.BlockEnvironment

trait BlockLike {
  var environment: BlockEnvironment
}

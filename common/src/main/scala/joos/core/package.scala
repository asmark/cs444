package joos

import java.io.Closeable
import scala.io.BufferedSource

package object core {
  /**
   * Make sure the underlying {@code resource} is disposed when the {@code block} ends
   */
  def using[R](resource: BufferedSource)(block: BufferedSource => R): R = {
    try {
      block(resource)
    } finally {
      if (resource != null) {
        resource.close()
      }
    }
  }

  def using[T <: Closeable, R](resource: T)(block: T => R): R = {
    try {
      block(resource)
    } finally {
      if (resource != null) {
        resource.close()
      }
    }
  }
}

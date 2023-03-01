package io.github.ablearthy.tl.codecs

import io.circe._
object TDJsonEncoder {
  def deriveProductEncoder[T](typeName: String, encoder: Encoder.AsObject[T]) =
    new Encoder.AsObject[T] {
      override def encodeObject(a: T): JsonObject = {
        ("@type" -> Json.fromString(typeName)) +: encoder.encodeObject(a)
      }
    }
}

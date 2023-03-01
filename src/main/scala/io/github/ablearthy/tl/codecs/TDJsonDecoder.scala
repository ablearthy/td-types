package io.github.ablearthy.tl.codecs

import io.circe._

object TDJsonDecoder {
  def deriveProductDecoder[T](typeName: String, decoder: Decoder[T]): Decoder[T] =
    new Decoder[T] {
      override def apply(c: HCursor): Decoder.Result[T] = {
        c.downField("@type").as[String].flatMap { s =>
          if (s == typeName) {
            decoder(c)
          } else {
            Left(DecodingFailure(s"unknown type", c.history :+ CursorOp.DownField("@type")))
          }
        }
      }
    }
}

package io.github.ablearthy.tl.codecs

import io.github.ablearthy.tl.aliases._
import io.circe._
import scala.util.Try
import java.util.Base64

import java.nio.charset.StandardCharsets

object BytesCodecs {
  val bytesDecoder: Decoder[Bytes] = Decoder.decodeString.emapTry { str =>
    Try(Base64.getDecoder().decode(str)).map(Bytes)
  }

  val bytesEncoder: Encoder[Bytes] = Encoder.encodeString.contramap[Bytes] { b =>
    Base64.getEncoder().encodeToString(b.underlying)
  }
}

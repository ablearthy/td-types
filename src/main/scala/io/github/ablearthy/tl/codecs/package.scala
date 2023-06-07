package io.github.ablearthy.tl

import io.github.ablearthy.tl.aliases.Bytes
import io.circe.{Decoder, Encoder}

package object codecs {
  implicit final val bytesDecoder: Decoder[Bytes] = BytesCodecs.bytesDecoder
  implicit final val bytesEncoder: Encoder[Bytes] = BytesCodecs.bytesEncoder
}

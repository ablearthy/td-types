package io.github.ablearthy.tl

package object codecs {
  implicit final val bytesDecoder = BytesCodecs.bytesDecoder
  implicit final val bytesEncoder = BytesCodecs.bytesEncoder
}

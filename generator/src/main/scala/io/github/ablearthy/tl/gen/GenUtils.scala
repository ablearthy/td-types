package io.github.ablearthy.tl.gen

import io.github.ablearthy.tl.parser.IdentWithNs
import io.github.ablearthy.tl.parser.LcIdentFull

object GenUtils {

  def translateConstructorName(lc: LcIdentFull): String =
    translateConstructorName(lc.namespace, lc.ident)

  def translateConstructorName(iwn: IdentWithNs): String =
    translateConstructorName(iwn.namespace, iwn.ident)

  def translateConstructorName(namespace: Option[String], name: String): String = {
    namespace match {
      case Some(ns) => s"${ns.capitalize}__${name.capitalize}"
      case None     => name.capitalize
    }
  }

  def lowercase(s: String): String = s.isEmpty match {
    case true => s
    case false => s(0).toLower + s.substring(1)
  }

  /** @param namespace
    *   namespace
    * @param name
    *   ident name
    * @return
    *   constructor name in a normal form (i.e. [namespace.]name)
    */
  def showConstructorName(namespace: Option[String], name: String): String = {
    namespace match {
      case Some(ns) => s"${ns}.${name}"
      case None     => name
    }
  }
}

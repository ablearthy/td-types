package io.github.ablearthy.tl.gen

import io.github.ablearthy.tl.parser.{Term => PTerm, _}

import scala.meta._

case class Gen(private val definitions: Vector[Definition]) {
  val types: Map[String, Vector[Definition]] = definitions.groupBy { definition =>
    val rtIdent = definition.resultType.ident
    GenUtils.translateConstructorName(namespace = rtIdent.namespace, name = rtIdent.ident)
  }

  val ctors = definitions.map { definition =>
    val cIdent = definition.constructorName
    GenUtils.showConstructorName(cIdent.namespace, cIdent.ident) -> definition
  }.toMap

  private val embeddedTypes = Map("double" -> "Double", "string" -> "String", "vector" -> "Vector")

  // mapping:
  // singleton? => use return-type
  // sum-type? => return-type ++ constructor-name
  val ctorToType: Map[String, String] = ctors.map { case (ctorName, definition) =>
    val rtIdent = definition.resultType.ident
    val name =
      GenUtils.translateConstructorName(namespace = rtIdent.namespace, name = rtIdent.ident)

    if (types(name).length == 1) {
      ctorName -> name
    } else {
      val internalName = GenUtils.translateConstructorName(
        namespace = definition.constructorName.namespace,
        name = definition.constructorName.ident
      )
      ctorName -> (name ++ internalName)
    }
  } ++ embeddedTypes

  def generateSingleton(
      definition: Definition,
      maybeExtends: Option[Type.Name] = None
  ): Defn.Class = {
    val scalaTypeName = ctorToType(
      GenUtils.showConstructorName(
        definition.constructorName.namespace,
        definition.constructorName.ident
      )
    )

    val typeParams = getTypeParams(definition.resultType)
    val realTypeParameters = typeParams.map(x => tparam"$x")
    val params = definition.args.zipWithIndex.flatMap { case (u, i) => generateParam(u, i) }.toList
    val name = Type.Name(scalaTypeName)

    maybeExtends match {
      case Some(ex) => {
        val t = typeParams match {
          case Nil => t"$ex"
          case _   => t"$ex[..$typeParams]"
        }
        val r = meta.Init(t, Name(""), Nil)
        q"case class $name[..$realTypeParameters](..$params) extends $r"
      }
      case None => q"case class $name[..$realTypeParameters](..$params)"
    }
  }

  def generateSum(base: String, definitions: Vector[Definition]): List[Defn] = {
    val name = Type.Name(base)
    val typeParams = getTypeParams(definitions(0).resultType).map(x => tparam"$x")
    val theTrait = q"sealed trait ${name}[..$typeParams]"
    theTrait :: definitions.map(d => generateSingleton(d, Some(name))).toList
  }

  def generateParam(arg: Args, index: Int): List[Term.Param] = arg match {
    case CondArg(ident, cond, _, typeTerm) => {
      val name = meta.Name(ident.getOrElse(s"anon__$index"))
      val scalaType = transformPTerm(typeTerm)
      if (cond.isDefined) {
        List(param"$name: Option[$scalaType]")
      } else {
        List(param"$name: $scalaType")
      }
    }
    case ArrayArg(ident, multiplicity, args) => {
      val rest = Type.Tuple(args.map { case AnonymousArg(_, term) => transformPTerm(term) }.toList)
      val name = meta.Name(ident.getOrElse(s"anon__$index"))
      List(param"$name: Vector[$rest]")
    }
    case BracketArg(idents, exclMark, typeTerm) => {
      val tpe = transformPTerm(typeTerm)
      val paramNames = idents.zipWithIndex.map { case (a, internalIndex) =>
        meta.Name(a.getOrElse(s"anon__${index}__$internalIndex"))
      }
      paramNames.map { name => param"$name: $tpe" }.toList
    }
    case AnonymousArg(exclMark, typeTerm) => {
      val name = meta.Name(s"anon__$index")
      List(param"$name: ${transformPTerm(typeTerm)}")
    }
  }

  def getTypeParams(resultType: ResultType): List[Type.Name] = {
    resultType.terms.map { case SimpleTerm(_, SimpleTypeIdent(IdentWithNs(_, n))) =>
      meta.Type.Name(n.capitalize)
    }.toList
  }

  def transformPTerm(term: PTerm): Type = term match {
    case BracketTerm(bare, fst +: rest) => {
      val tpe = transformPTerm(fst)
      val cons = rest.map(transformPTerm).toList
      t"$tpe[..$cons]"
    }
    case SimpleTerm(bare, s) => transformTypeIdent(bare, s)
    case AngleBracketTerm(bare, typeIdent, rest) => {
      val tpe = transformTypeIdent(bare, typeIdent)
      val cons = rest.map(transformPTerm).toList
      t"$tpe[..$cons]"
    }
  }

  def transformTypeIdent(bare: Boolean, typeIdent: TypeIdent): Type.Name = typeIdent match {
    case SimpleTypeIdent(IdentWithNs(namespace, ident)) => {
      if (ident == "Vector") {
        Type.Name("Vector")
      } else if (ident(0).isLower) {
        val name = GenUtils.showConstructorName(namespace, ident)
        ctorToType.get(name).map(x => Type.Name(x)).getOrElse(Type.Name(name.capitalize))
      } else {
        Type.Name(GenUtils.translateConstructorName(namespace = namespace, name = ident))
      }
    }
    case HashTypeIdent => Type.Name("Int")
  }

  def generateCodecsForProduct(definition: Definition): (Defn, Defn) = {
    val rawName = GenUtils.showConstructorName(
      namespace = definition.constructorName.namespace,
      name = definition.constructorName.ident
    )
    val name = ctorToType(rawName)
    val realName = name(0).toLower + name.substring(1)

    val encoderAsObject = t"Encoder.AsObject[${Type.Name(name)}]"
    val decoderT = t"Decoder[${Type.Name(name)}]"
    val someName = Lit.String(rawName(0).toLower + rawName.substring(1))
    val r = Pat.Var(Term.Name(realName + "Encoder"))
    val r1 = Pat.Var(Term.Name(realName + "Decoder"))
    val qEncoder =
      q"lazy implicit val $r: $encoderAsObject = TDJsonEncoder.deriveProductEncoder($someName, deriveEncoder)"
    val qDecoder =
      q"lazy implicit val $r1: $decoderT = TDJsonDecoder.deriveProductDecoder($someName, deriveDecoder)"
    (qEncoder, qDecoder)
  }

  def generateDecoderForSum(base: String, definitions: Vector[Definition]): Defn = {
    val decoderName = Pat.Var(Term.Name({ base(0).toLower + base.substring(1) } + "Decoder"))
    val decoderT = t"Decoder[${Type.Name(base)}]"
    val defs = definitions.map { definition =>
      val rawName = GenUtils.showConstructorName(
        namespace = definition.constructorName.namespace,
        name = definition.constructorName.ident
      )
      val name = ctorToType(rawName)
      val realName = name(0).toLower + name.substring(1)
      val x = Term.Name(realName + "Decoder")
      q"$x.widen"
    }.toList

    q"lazy implicit val $decoderName: $decoderT = List[$decoderT](..$defs).reduceLeft(_ or _)"
  }
  def generateEncoderForSum(base: String, definitions: Vector[Definition]): Defn = {
    val encoderName = Pat.Var(Term.Name({
      base(0).toLower + base.substring(1)
    } + "Encoder"))
    val encoderT = t"Encoder.AsObject[${Type.Name(base)}]"
    val defs = definitions.map { definition =>
      val rawName = GenUtils.showConstructorName(
        namespace = definition.constructorName.namespace,
        name = definition.constructorName.ident
      )
      val name = ctorToType(rawName)
      val realName = name(0).toLower + name.substring(1)
      val x = Term.Name(realName + "Encoder")
      p"case a: ${Type.Name(name)} => $x.encodeObject(a)"
    }.toList
    q"lazy implicit val $encoderName: $encoderT = Encoder.AsObject.instance { ..case $defs }"
  }

}

object Gen {
  def main(args: Array[String]): Unit = {
    val x = Term.Name("type_")
    val params: List[Type.Param] = List.empty
    println(q"case class X[..$params]()")
  }
}

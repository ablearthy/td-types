import fastparse.{Parsed, parse}
import io.github.ablearthy.tl.parser.schemaParser
import io.github.ablearthy.tl.parser.CondArg
import io.github.ablearthy.tl.gen.Gen

import scala.meta._

object Main {

  def main(args: Array[String]): Unit = {
    val td_api = getClass.getResourceAsStream("td_api.tl")
    val result =
      parse(
        td_api,
        schemaParser(_)
      )
    result match {
      case Parsed.Success(r, _) => {
        val gen = Gen(r.types)
        val defs = gen.types
          .map { case (k, v) =>
            if (v.length == 1) { List(gen.generateSingleton(v(0))) }
            else { gen.generateSum(k, v) }
          }
          .flatten
          .toList
        println(source"..$defs")
      }

      case failure: Parsed.Failure => println(s"Failure: $failure")
    }
  }
}

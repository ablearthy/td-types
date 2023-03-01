import fastparse.{Parsed, parse}
import io.github.ablearthy.tl.parser.schemaParser

object Main {

  def main(args: Array[String]): Unit = {
    val td_api = getClass.getResourceAsStream("td_api.tl")
    val result =
      parse(
        td_api,
        schemaParser(_)
      )
    result match {
      case Parsed.Success(r, _) =>
        println("Types:")
        r.types.foreach { definition =>
          println(s"${definition.constructorName} ${definition.resultType}")
        }
      case failure: Parsed.Failure => println(s"Failure: $failure")
    }
  }
}


package jsonp4s
import scala.util.parsing.combinator._

object JSONParser extends JavaTokenParsers {
  def obj: Parser[Map[String, Any]] = "{" ~> repsep(member, ",") <~ "}" ^^ { Map() ++ _ }
  def arr: Parser[List[Any]] = "[" ~> repsep(value, ",") <~ "]"
  def member: Parser[(String, Any)] = stringLiteral ~ ":" ~ value ^^ {
    case name ~ ":" ~ value => (name, value)
  }
  def value: Parser[Any] = (
    obj
  | arr
  | stringLiteral
  | floatingPointNumber ^^ { _.toDouble }
  | "null"  ^^^ null
  | "true"  ^^^ true
  | "false" ^^^ false
  )

  def apply(str: String) = parseAll(value, str)
}


object JSONDump {
  import scala.io.Source
  def main(args: Array[String]): Unit = {
    if (args.length == 0) println("Usage: scala jsonp4s.JSONDump <file>")
    else {
      val src = Source.fromFile(args(0)).getLines().mkString
      val json = JSONParser(src)
      println(json)
    }
  }
}

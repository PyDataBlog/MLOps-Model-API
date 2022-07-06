package scala.in.programming.function_and_closure

import scala.io.Source

/**
  * @author loustler
  * @since 02/03/2017 21:28
  */
object LongLines {
  def processFile(filename: String, width: Int): Unit = {
    val source = Source.fromFile(filename)

    for ( line <- source.getLines() )
      processLine(filename, width, line)
  }

  def processLine(filename: String, width: Int, line: String): Unit = {
    if ( line.length > width )
      println(filename + " : " + line.trim)
  }

  /**
    * This function use local function literal.
    *
    * This function same work [[LongLines.processFile()]] and [[LongLines.processLine()]] perfectly.
    *
    * The difference thing just use function literal used.
    *
    * @param filename
    * @param width
    */
  def processFileByLiteral(filename: String, width: Int): Unit = {
    def processLine(filename: String, width: Int, line: String): Unit = {
      if ( line.length > width )
        println(filename + " : " + line.trim)
    }

    val source = Source.fromFile(filename)

    for ( line <- source.getLines() )
      processLine(filename, width, line)
  }

  def main(args: Array[String]): Unit = {
    val width = args(0).toInt

    for ( arg <- args.drop(1) )
      LongLines.processFile(arg, width)
  }
}

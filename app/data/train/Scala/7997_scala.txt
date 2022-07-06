#!/usr/bin/env scala

import scala.io.StdIn
import scala.sys.process.Process
import scala.language.postfixOps

while (true) {
  val input = StdIn.readLine("$ ")
  // exit
  if (
    input.split("\\s+") match {
      case l =>
        l.length != 0 && l.head == "exit"
    }
  ) sys.exit(0)
  val proc  = Process(List("sh", "-c", input))

  // run
  val status = proc!
}

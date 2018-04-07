package Bot

import scala.io.Source

object App {

  def main(args: Array[String]) {
    val parser = CommandParser

    val filename = "input.txt"

    for (line <- Source.fromFile(filename).getLines) {
      parser.apply(line.trim)
    }
  }




}

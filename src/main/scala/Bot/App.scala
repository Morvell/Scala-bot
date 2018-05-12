package Bot

import java.util.Date

import scala.io.Source

object App {

  def responseToLine(line: String): String = {
    val result = CommandParser(line.trim)

    result.map {
      case c: Matcher.CreatePoolM => CommandImpl.createPoll(c.name,c.anon,c.cont,c.start,c.stop)
      case c: Matcher.ListM => CommandImpl.listPolls()
      case c: Matcher.DeleteM => CommandImpl.deletePoll(c.d)
      case c: Matcher.StartM => CommandImpl.startPoll(c.d, new Date())
      case c: Matcher.StopM => CommandImpl.stopPoll(c.d, new Date())
      case c: Matcher.ResultM => CommandImpl.pollResult(c.d)
    } match {
      case CommandParser.Success(response, _) => response.toString
      case CommandParser.Failure(error, _) => s"Error: $error"
    }
  }

  def main(args: Array[String]) {
    for (line <- Source.fromFile("input.txt").getLines) {
      println(responseToLine(line))
    }
  }
}

package Bot

import java.util.Date

import scala.io.Source
import scala.util.Failure

object App {

  def main(args: Array[String]) {
    val parser = CommandParser

    val filename = "input.txt"

    for (line <- Source.fromFile(filename).getLines) {
      val result = parser(line.trim)

      val prepared = result.map {
          case c: Matcher.CreatePoolM => CommandImpl.createPoll(c.name,c.anon,c.cont,c.start,c.stop)
          case c: Matcher.ListM => CommandImpl.listPolls()
          case c: Matcher.DeleteM => CommandImpl.deletePoll(c.d)
          case c: Matcher.StartM => CommandImpl.startPoll(c.d, new Date())
          case c: Matcher.StopM => CommandImpl.stopPoll(c.d, new Date())
          case c: Matcher.ResultM => CommandImpl.pollResult(c.d)
      }

      prepared match {
        case parser.Success(response, _) => CommandImpl.worker(response.toString)
        case parser.Failure(error, _) => CommandImpl.worker(s"Error: $error")
      }

    }
  }




}

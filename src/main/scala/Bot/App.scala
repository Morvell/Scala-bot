package Bot

import java.util.Date

import scala.io.Source

object App {

  def responseToLine(line: String): String = {
    val result = CommandParser(line.trim)

    result.map {
      case c: Matcher.CreatePoolM => CommandImpl.createPoll(c.name,c.anon,c.cont,c.start,c.stop)
      case c: Matcher.ListM => CommandImpl.listPolls()
      case c: Matcher.DeleteM => CommandImpl.deletePoll(c.d, CommandImpl.getUserID)
      case c: Matcher.StartM => CommandImpl.startPoll(c.d, new Date(), CommandImpl.getUserID)
      case c: Matcher.StopM => CommandImpl.stopPoll(c.d, new Date(), CommandImpl.getUserID)
      case c: Matcher.ResultM => CommandImpl.pollResult(c.d)
      case c: Matcher.BeginM => CommandImpl.begin(c.d)
      case c: Matcher.EndM => CommandImpl.end()
      case c: Matcher.AddQuestionOpenM => CommandImpl.addQuestion(c.q,c.t,"open"::Nil, CommandImpl.getUserID)
      case c: Matcher.AddQuestionChoiceM => CommandImpl.addQuestion(c.q,c.t,c.variant, CommandImpl.getUserID)
      case c: Matcher.DeleteQuestionM => CommandImpl.deleteQuestion(c.d, CommandImpl.getUserID)
      case c: Matcher.AnswerStringM => CommandImpl.addAnswerOpen(c.d,c.a)
      case c: Matcher.AnswerDigitM => CommandImpl.addAnswerChoice(c.d,c.a)
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

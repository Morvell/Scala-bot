package Bot

import java.util.Date

object App {
  def responseToLine(line: String, user: User): String = {
    val result = CommandParser(line.trim)
    result.map {
      case c: Matcher.CreatePollM => CommandImpl.createPoll(c.name,c.anon,c.cont,c.start,c.stop, user)
      case c: Matcher.ListM => CommandImpl.listPolls()
      case c: Matcher.DeleteM => CommandImpl.deletePoll(c.d,user)
      case c: Matcher.StartM => CommandImpl.startPoll(c.d, new Date(), user)
      case c: Matcher.StopM => CommandImpl.stopPoll(c.d, new Date(), user)
      case c: Matcher.ResultM => CommandImpl.pollResult(c.d)
      case c: Matcher.BeginM => CommandImpl.begin(c.d, user)
      case c: Matcher.EndM => CommandImpl.end(user)
      case c: Matcher.AddQuestionOpenM => CommandImpl.addQuestion(c.q,c.t,"open"::Nil, user)
      case c: Matcher.AddQuestionChoiceM => CommandImpl.addQuestion(c.q,c.t,c.variant, user)
      case c: Matcher.DeleteQuestionM => CommandImpl.deleteQuestion(c.d, user)
      case c: Matcher.AnswerStringM => CommandImpl.addAnswerOpen(c.d,c.a, user)
      case c: Matcher.AnswerDigitM => CommandImpl.addAnswerChoice(c.d,c.a, user)
      case c: Matcher.ViewM => CommandImpl.view(user)
      case c: Matcher.PrintHelpM => CommandImpl.printHelp()
    } match {
      case CommandParser.Success(response, _) => response.toString
      case CommandParser.Failure(text, _) =>
        if (text.contains("expected")) s"```$text```" else text
      case CommandParser.Error(text, _) =>
        if (text.contains("expected")) s"```$text```" else text
    }
  }
}

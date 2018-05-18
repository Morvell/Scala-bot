package Bot

import java.util.Date

object App {

  def responseToLine(line: String, user: User): String = {
    val result = CommandParser(line.trim)
    result.map {
      case c: Matcher.CreatePoolM =>
        CommandImpl.createPollView(CommandImpl.createPoll(c.name,c.anon,c.cont,c.start,c.stop, user), c.name)
      case c: Matcher.ListM => CommandImpl.listPolls()
      case c: Matcher.DeleteM => CommandImpl.deletePoll(c.d,user)
      case c: Matcher.StartM => CommandImpl.startPoll(c.d, new Date(), user)
      case c: Matcher.StopM => CommandImpl.stopPoll(c.d, new Date(), user)
      case c: Matcher.ResultM => CommandImpl.pollResult(c.d)
      case c: Matcher.BeginM => CommandImpl.begin(c.d)
      case c: Matcher.EndM => CommandImpl.end()
      case c: Matcher.AddQuestionOpenM => CommandImpl.addQuestion(c.q,c.t,"open"::Nil, user)
      case c: Matcher.AddQuestionChoiceM => CommandImpl.addQuestion(c.q,c.t,c.variant, user)
      case c: Matcher.DeleteQuestionM => CommandImpl.deleteQuestion(c.d, user)
      case c: Matcher.AnswerStringM => CommandImpl.addAnswerOpen(c.d,c.a, user)
      case c: Matcher.AnswerDigitM => CommandImpl.addAnswerChoice(c.d,c.a, user)
      case c: Matcher.ViewM => CommandImpl.view()
      case c: Matcher.PrintHelpM => s"👾 *Available commands:*" +
        s"\n/create\\_poll - create new poll" +
        s"\n/list - list current polls" +
        s"\n/delete\\_poll - delete poll" +
        s"\n/start\\_poll - start poll" +
        s"\n/stop\\_poll - stop poll" +
        s"\n/result - view poll results" +
        s"\n/begin - start working with poll" +
        s"\n\n👾 After */begin*, these commands will be available: 👇" +
        s"\n/view - view all poll questions" +
        s"\n/add\\_question - add question to the poll" +
        s"\n/delete\\_question - delete question" +
        s"\n/answer - answer to the question"
    } match {
      case CommandParser.Success(response, _) => response.toString
      case CommandParser.Failure(text, _) => text
      case CommandParser.Error(text, _) => text
    }
  }
}

package Bot


object Answers {
  def noSuchPoll = s"Can't find such poll 👻. Maybe it doesn't exist?"
  def noPermissions = s"😟 Sorry, you don't have enough permissions for doing this"
  def youForgotToBegin = s"Ah, you probably forgot to /begin 😌"
  def noPolls = s"😐 No polls created yet."
  def noSuchQuestion = s"🎃 No such question 🎃"
  def listPolls(polls: Iterable[Poll]): String = {
    val pollStrings = polls.map(poll => s"*${poll.id})* ${poll.name}")
    s"👉 Current polls:\n${pollStrings.mkString("\n")}"
  }
  def pollWasDeleted = s"Poll was deleted successfully 😈"
  def pollIsRunning = s"👌 Poll is running"
  def pollWasStarted = s"🤘 Poll was started"
  def cantStartPoll(id: Int) = s"Can't start poll *$id* for some reason 😕"
  def pollIsNotActive = s"Poll isn't active 😤. Be patient."
  def pollIsStopped = s"Poll is stopped ⛔"
  def pollWillStartAutomatically = s"Don't worry, poll will stop automatically 😉"
  def afterBeginHint: String = s"🤓 Okay, now you can:" +
    s"\n/add\\_question _(<question>)_ _(open|choice|multi)_," +
    s"\n/delete\\_question _(<question number>)_," +
    s"\n/answer _(<question number>)_ _(<answer>)_ or" +
    s"\n/view all questions" +
    s"\nAnd don't forget to /end 😉"
  def afterEndHint(pollId: Int) = s"🙌 Wow, now you can */result ($pollId)* 🙌"
  def questionWasAdded(name: String, id: Int) = s"👌 Question _'$name'_ was added *($id)*"
  def questionWasDeleted = s"🤞 Question was deleted"
  def cantVoteTwice = s"Hey, you can't vote twice! 🇷🇺"
  def badQuestionType(typeOfQuestion: String) = s"😤 Nah, this is a *$typeOfQuestion* question"
  def thanksForVoting = "✔ Thank you for voting"
  def pollWasCreated(id: Int, name: String): String =
    s"😇 Poll *$name* was created, here is your poll id:\n👉 `$id` 👈\n" +
      s"🦄 Type */begin ($id)* to continue"
}

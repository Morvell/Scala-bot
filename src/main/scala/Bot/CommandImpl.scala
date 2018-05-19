package Bot

import java.text.SimpleDateFormat
import java.util.Date

import Bot.CommandImpl.getContextById

import scala.collection.immutable
import scala.collection.immutable.HashSet


trait Repository {
  var polls: Map[Int, Poll] = immutable.Map[Int, Poll]()

  def updatePoll(pollId: Int, poll: Poll) {
    polls += pollId -> poll
  }

  def removePoll(pollId: Int) {
    polls -= pollId
  }

  def getPollById(id: Int): Either[String, Poll] =
    if (polls.contains(id)) Right(polls(id)) else Left(Answers.noSuchPoll)

  def getPollForUser(pollId: Int, userId: Long): Either[String, Poll] = {
    getPollById(pollId).flatMap(poll =>
      if (poll.admin == userId) Right(poll) else Left(Answers.noPermissions))
  }

  def getQuestionForUser(questionId: Int, userId: Long): Either[String, (Poll, Question)] =
    getContextById(userId).flatMap(getPollById).flatMap(poll =>
      if (questionId < 0 || questionId >= poll.questions.size) Left(Answers.noSuchQuestion)
      else Right((poll, poll.questions(questionId))))
}

trait Context {
  var context: Map[Long, Int] = immutable.Map[Long, Int]()

  def setContext(userId: Long, context: Int) {
    this.context += userId -> context
  }

  def removeContext(userId: Long) {
    context -= userId
  }

  def isInContext(userId: Long): Boolean = context.contains(userId)

  def getContextById(id: Long): Either[String, Int] = {
    if (!context.contains(id)) Left(Answers.youForgotToBegin)
    else Right(context(id))
  }
}


object CommandImpl extends Repository with Context {

  val formatDate = new SimpleDateFormat("hh:mm:ss yy:MM:dd")

  val maxId: Iterator[Int] = Stream.from(0).iterator

  val userID: Iterator[Int] = Stream.from(0).iterator

  def getMaxID: Int = {
    maxId.next()
  }

  def parseTime(time: Option[String]): Option[Date] = {
    if (time.isDefined)
      Option(formatDate.parse(time.getOrElse(formatDate.format(new Date))))
    else
      None
  }

  def getTimeFromFormat(string: String): Date = {
    formatDate.parse(string)
  }

  def createPoll(name: String, anonymityVar: Option[String], continuousOrAfterstopVar: Option[String],
                 startTimeVar: Option[String], stopTimeVar: Option[String], user: User = User(0, "")): Int = {
    val anonymity = anonymityVar.getOrElse("yes") == "yes"

    val continuousOrAfterstop = continuousOrAfterstopVar.getOrElse("afterstop") == "continuous"

    val id = getMaxID

    updatePoll(id, Poll(name, id, user.id, anonymity,
      continuousOrAfterstop, parseTime(startTimeVar), parseTime(stopTimeVar)))

    id
  }

  def createPollView(id: Int, name: String): String =
    s"😇 Poll *$name* was created, here is your poll id:\n👉 `$id` 👈\n" +
      s"🦄 Type */begin ($id)* to continue"

  def listPolls(): String =
    if (polls.isEmpty)
      Answers.noPolls
    else
      s"👉 Current polls:\n${polls.aggregate("")((s, p) => s"$s *${p._1})* ${p._2.name}\n", _ + _)}"

  def deletePoll(id: Int, user: User): String =
    getPollForUser(id, user.id).map(_ => {
      removePoll(id)
      s"Poll deleted successfully 😈"
    }).merge

  def startPoll(id: Int, date: Date, user: User): String =
    getPollForUser(id, user.id).map(poll => {
      if (PollCommand.active(poll, date) || poll.start_time.isDefined) {
        s"👌 Poll is running"
      } else if (poll.start_time.isEmpty) {
        updatePoll(id, PollCommand.start(poll, date))
        s"🤘 Poll has started"
      } else s"Can't start poll *$id* for some reason 😕"
    }).merge

  def stopPoll(id: Int, date: Date, user: User): String =
    getPollForUser(id, user.id).map(poll => {
      if (!PollCommand.active(poll, date)) {
        s"Poll isn't active 😤. Be patient."
      } else if (poll.end_time.isEmpty) {
        updatePoll(id, PollCommand.stop(poll, date))
        s"Poll is stopped ⛔"
      }
      else {
        s"Don't worry, poll will stop automatically 😉"
      }
    }).merge

  def pollResult(id: Int): String =
    getPollById(id).map(poll => PollCommand.getResult(poll, new Date)).merge

  def begin(id: Int, user: User): String =
    getPollById(id).map(_ => {
      setContext(user.id, id)
      s"🤓 Okay, now you can:" +
        s"\n/add\\_question _(<question>)_ _(open|choice|multi)_," +
        s"\n/delete\\_question _(<question number>)_," +
        s"\n/answer _(<question number>)_ _(<answer>)_ or" +
        s"\n/view all questions" +
        s"\nAnd don't forget to /end 😉"
    }).merge

  def end(user: User): String =
    getContextById(user.id).map(pollId => {
      removeContext(user.id)
      s"Wow, now you can */result ($pollId)*"
    }).merge

  def view(user: User): String =
    getContextById(user.id).flatMap(getPollById).map(PollCommand.getView).merge

  def addQuestion(name: String, typeOfQuestion: String, list: List[String], user: User): String =
    getContextById(user.id)
      .flatMap(pollId => getPollForUser(pollId, user.id))
      .map(poll => {
        val question = Question(name, typeOfQuestion, HashSet[User](), list.map(e => Variant(e, Nil)))
        updatePoll(poll.id, PollCommand.addQuestion(poll, question))
        s"👌 Question _'$name'_ was added *(${poll.questions.size})*"
      }).merge

  def deleteQuestion(questionId: Int, user: User): String =
    getQuestionForUser(questionId, user.id).map {
      case (poll, _) =>
        updatePoll(poll.id, PollCommand.deleteQuestionById(poll, questionId))
        s"🤞 Question was deleted"
    }.merge

  def addAnswerOpen(questionId: Int, answer: String, user: User): String =
    getQuestionForUser(questionId, user.id).map {
      case (poll, question) =>
        if (question.voitedUsers.contains(user))
          s"Hey, you can't vote twice! 🇷🇺"
        else if (question.typeOfQuestion != "open")
          s"😤 Nah, this is a *${question.typeOfQuestion}* question"
        else {
          val updatedQuestion = QuestionHandler.addAnswer(poll.questions(questionId),
            poll.anonymity, 0, Answer(answer, Some(user)))
          updatePoll(poll.id, PollCommand.updateQuestion(poll, questionId, b))
          "✔ Thank you for voting"
        }
    }.merge

  def addAnswerChoice(questionId: Int, list: List[Int], user: User): String =
    getQuestionForUser(questionId, user.id).map {
      case (poll, question) =>
        if (question.voitedUsers.contains(user))
          s"Hey, you can't vote twice! 🇷🇺"
        else if (question.typeOfQuestion == "choice" && list.size > 1)
          s"😤 Nah, this is a *${question.typeOfQuestion}* question." +
            s"You can take only 1️⃣ option"
        else {
          for (i <- list) yield {
            val updatedQuestion = QuestionHandler.addAnswerMulti(question, poll.anonymity, i, Answer("", Option(user)))
            updatePoll(poll.id, PollCommand.updateQuestion(poll, questionId, updatedQuestion))
          }
          val updatedQuestion = QuestionHandler.addInVoitedUser(question, Answer("", Option(user)))
          updatePoll(poll.id, PollCommand.updateQuestion(poll, questionId, updatedQuestion))
          "✔ Thank you for voting"
        }
    }.merge

  def printHelp(): String = {
    s"👾 *Available commands:*" +
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
      s"\n/answer - answer to the question" +
      s"\n/end - leave current poll"
  }

}

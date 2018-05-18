package Bot

import java.text.SimpleDateFormat
import java.util.Date

import scala.collection.immutable
import scala.collection.immutable.HashSet


trait Repository {
  var polls: Map[Int, Poll] = immutable.Map[Int, Poll]()

  def putInRep(id: Int, poll: Poll) {
    polls = polls + (id -> poll)
  }

  def getRep: Map[Int, Poll] = polls

  def removeFromRep(id: Int) {
    polls = polls - id
  }

  def cleanRep() {
    polls = polls.empty
  }

  def getPollById(id: Int): Poll = {
    polls(id)
  }

  def getPollByIdOption(id: Int): Option[Poll] = {
    polls.get(id)
  }
}

case class Context(context:Option[Int])

object CommandImpl extends Repository {


  val formatDate = new SimpleDateFormat("hh:mm:ss yy:MM:dd")

  val maxId = Stream.from(0).iterator

  val userID = Stream.from(0).iterator //TODO временно

  var context:Option[Int] = None

  def getMaxID: Int = {
    maxId.next()
  }

  def startTime(time: Option[String]): Option[Date] = {
    if (time.isDefined) {
      return Option(formatDate.parse(time.getOrElse(formatDate.format(new Date))))
    }

    None
  }

  def getTimeFromFormat(string: String): Date = {
    formatDate.parse(string)
  }

  def stopTime(time: Option[String]): Option[Date] = {
    if (time.isDefined) {
      return Option(formatDate.parse(time.getOrElse(formatDate.format(new Date))))
    }
    None
  }

  def checkRoot(poll: Poll, id:Long):Boolean = {
    poll.admin == id
  }

  def createPoll(name: String, anonymityVar: Option[String], continuousOrAfterstopVar: Option[String],
                 startTimeVar: Option[String], stopTimeVar: Option[String], user: User = User(0,"")): Int = {
    val anonymity = anonymityVar.getOrElse("yes") == "yes"

    val continuousOrAfterstop = continuousOrAfterstopVar.getOrElse("afterstop") == "continuous"

    val startTime1 = startTime(startTimeVar)

    val stopTime1 = stopTime(stopTimeVar)

    val id = getMaxID

    putInRep(id, Poll(name,id, user.id, anonymity, continuousOrAfterstop, startTime1, stopTime1))

    id
  }

  def createPollView(id: Int, name: String): String = {
    s"😇 Poll *$name* was created, here is your poll id:\n👉 `$id` 👈\n" +
      s"🦄 Type */begin ($id)* to continue"
  }

  def listPolls(): String = {
    if (getRep.isEmpty)
      return s"😐 No polls created yet."
    s"👉 Current polls:\n${getRep.aggregate("")((s, p) => s"$s *${p._1})* ${p._2.name}\n", _ + _)}"
  }

  def deletePoll(id: Int, userIDE: User): String = {
    getRep.get(id).map { poll =>
      if (!checkRoot(poll, userIDE.id))
        return s"😟 Sorry, you don't have enough permissions for doing this"
      removeFromRep(id)
      s"Poll deleted successfully 😈"
    }.getOrElse(s"Can't find such poll 👻. Maybe it doesn't exist?")
  }

  def startPoll(id: Int, date: Date, userIDE:User): String = {
    getPollByIdOption(id).map { poll =>
      if (!checkRoot(poll, userIDE.id))
        return s"😟 Sorry, you don't have enough permissions for doing this"
      if (PollCommand.active(poll, date) || poll.start_time.isDefined) {
        return s"👌 Poll is running"
      }

      if (poll.start_time.isEmpty) {
        putInRep(id, PollCommand.start(poll, date))
        return s"🤘 Poll has started"
      }
      return s"Can't start poll *$id* for some reason 😕"

    }.getOrElse(s"Can't find such poll 👻. Maybe it doesn't exist?")
  }


  def stopPoll(id: Int, date: Date, userIDE:User): String = {
    getPollByIdOption(id).map { poll =>
      if (!checkRoot(poll, userIDE.id))
        return s"😟 Sorry, you don't have enough permissions for doing this"
      if (!PollCommand.active(poll, date)) {
        return s"Poll isn't active 😤. Be patient."
      }
      if (poll.end_time.isEmpty) {
        putInRep(id, PollCommand.stop(poll, date))
        return s"Poll is stopped ⛔"
      }
      else {
        return s"Don't worry, poll will stop automatically 😉"
      }
      putInRep(id, PollCommand.stop(poll, date))
      return s"Poll was stopped ⛔"

    }.getOrElse(s"Can't find such poll 👻. Maybe it doesn't exist?")

  }


  def pollResult(id: Int): String = {
    getRep.get(id).map { poll =>
      PollCommand.getResult(getPollById(id), new Date)
    }.getOrElse(s"Can't find such poll 👻. Maybe it doesn't exist?")
  }

  def begin(id: Int): String = {
    if (!getRep.contains(id))
      return s"Can't find such poll 👻. Maybe it doesn't exist?"
    context = Option(id)
    s"🤓 Okay, now you can:" +
      s"\n*/add_question* _(<question>)_ _(open|choice|multi)_," +
      s"\n*/delete_question* _(<question number>)_," +
      s"\n*/answer* _(<question number>)_ _(<answer>)_ or */view* results." +
      s"\nAnd don't forget to */end* 😉"
  }

  def end(): String = {
    context.map { id =>
      context = None
      s"🤓 You can try typing */result* ($id)*"
    }.getOrElse(s"Ah, you probably forgot to */begin* 😌.")
  }

  def view(): String = {
    getPollByIdOption(context.getOrElse(return s"Ah, you probably forgot to */begin* 😌.")).map { poll =>
      PollCommand.getView(poll)

    }.getOrElse(s"Can't find such poll 👻. Maybe it doesn't exist?")
  }

  def addQuestion(name: String, typeOfQuestion: String, list: List[String], userIDE:User): String = {
    getPollByIdOption(context.getOrElse(return s"Ah, you probably forgot to */begin* 😌.")).map { poll =>
      if (!checkRoot(poll, userIDE.id))
        return s"😟 Sorry, you don't have enough permissions for doing this"
      val question = Question(name,typeOfQuestion, HashSet[User](),list.map(e => Variant(e, Nil)))
      putInRep(context.get, PollCommand.addQuestion(poll, question))
      s"👌 Question _'$name'_ was added *(${poll.questions.size})*"

    }.getOrElse(s"Can't find such poll 👻. Maybe it doesn't exist?")
  }

  def deleteQuestion(id:Int, userIDE:User): String = {
    getPollByIdOption(context.getOrElse(return s"Ah, you probably forgot to */begin* 😌.")).map { poll =>
      if (!checkRoot(poll, userIDE.id))
        return s"😟 Sorry, you don't have enough permissions for doing this"
      putInRep(context.get, PollCommand.deleteQuestionById(poll,id))
      s"🤞 Question was deleted"

    }.getOrElse(s"Can't find such poll 👻. Maybe it doesn't exist?")
  }

  def addAnswerOpen(id:Int, answer:String, user: User): String = {
    context.map(cont => {
      val poll = getPollByIdOption(cont).get
      if (poll.questions(id).voitedUsers.contains(user))
        return s"Hey, you can't vote twice! 🇷🇺"
      if (poll.questions(id).typeOfQuestion != "open")
        return s"😤 Nah, this is a *${poll.questions(id).typeOfQuestion}* question"
      val b = QuestionHandler.addAnswer(poll.questions(id),poll.anonymity, 0, Answer(answer,Option(user)))
      val a = PollCommand.updateQuestion(poll, id, b)
      putInRep(cont, a)
      "✔ Thank you for voting"

    }).getOrElse(s"Ah, you probably forgot to */begin* 😌.")
  }

  def addAnswerChoice(id:Int, list: List[Int], user: User): String = {
    context.map(cont => {
      for(i <- list) yield {
        val poll = getPollByIdOption(cont).get
        if (poll.questions(id).voitedUsers.contains(user))
          return s"Hey, you can't vote twice! 🇷🇺"
        if (poll.questions(id).typeOfQuestion == "choice" && list.size>1)
          return s"😤 Nah, this is a *${poll.questions(id).typeOfQuestion}* question." +
            s"You can take only 1️⃣ option"
        val b = QuestionHandler.addAnswer(poll.questions(id),poll.anonymity,  i, Answer("",Option(user)))
        val a = PollCommand.updateQuestion(poll, id, b)
        putInRep(cont, a)
      }
      "✔ Thank you for voting"
    }).getOrElse(s"Ah, you probably forgot to */begin* 😌.")
  }
}

package Bot

import java.text.SimpleDateFormat
import java.util.Date

import scala.collection.immutable.HashSet
import scala.collection.{immutable, mutable}


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

  def getPoolByIdOption(id: Int): Option[Poll] = {
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

  def worker(string: String): Unit = {
    println(string)
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


    id //TODO более отзывчиво
  }

  def listPolls(): String = {
    val a = getRep.getOrElse(0, return "Еще нет пулов")
    getRep.aggregate("Current polls: \n")((s, p) => s"$s ${p._1} :   ${p._2.name}\n", _ + _)
  }

  def deletePoll(id: Int, userIDE: User): String = {
    if (getRep.isEmpty) return "Тебе еще рано об этом задумываться"
    getRep.get(id).map { poll =>
      if (!checkRoot(poll, userIDE.id)) return "У тебя здесь нет прав"
      removeFromRep(id)
      "Poll deleted successfully"
    }.getOrElse("Error: poll is not exist")

  }

  def startPoll(id: Int, date: Date, userIDE:User): String = {
    getPoolByIdOption(id).map { poll =>
      if (!checkRoot(poll, userIDE.id)) return "У тебя здесь нет прав"
      if (PollCommand.active(poll, date)) {
        return "Уже запущен"
      }

      if (poll.start_time.isDefined) return "Уже запущен"

      if (poll.start_time.isEmpty) {
        putInRep(id, PollCommand.start(poll, date))
        return "The poll is started successfully"
      }
      return "Error"

    }.getOrElse("Error : poll is not exist")

  }


  def stopPoll(id: Int, date: Date, userIDE:User): String = {

    getPoolByIdOption(id).map { poll =>
      if (!checkRoot(poll, userIDE.id)) return "У тебя здесь нет прав"
      if (!PollCommand.active(poll, date)) {
        return "Опрос еще не запущен"
      }
      if (poll.end_time.isEmpty) {
        putInRep(id, PollCommand.stop(poll, date))
        return "The poll is stopped successfully"
      }
      else {
        return "Error: опрос остановится автоматически"
      }
      putInRep(id, PollCommand.stop(poll, date))
      return "The poll is stopped successfully"

    }.getOrElse("Error: poll is not exist")

  }


  def pollResult(id: Int): String = {
    getRep.get(id).map { poll =>
      PollCommand.getResult(getPollById(id), new Date)
    }.getOrElse("Error: poll is not exist")
  }

  def begin(id: Int): String = {
    if (!getRep.contains(id)) return "Нет такого контекста"
    context = Option(id)
    "Контекст переключен на " + id
  }

  def end(): String = {
    context.map { a =>
      context = None
      "Контекст отключен"
    }.getOrElse("Контекст не известен")
  }

  def view(): String = {
    getPoolByIdOption(context.getOrElse(return "Тебе еще рано об этом задумываться")).map { poll =>
      PollCommand.getView(poll)

    }.getOrElse("Error : не выбран контекст")
  }

  def addQuestion(name: String, typeOfQuestion: String, list: List[String], userIDE:User): String = {
    getPoolByIdOption(context.getOrElse(return "Тебе еще рано об этом задумываться")).map { poll =>
      if (!checkRoot(poll, userIDE.id)) return "У тебя здесь нет прав"
      val question = Question(name,typeOfQuestion, HashSet[User](),list.map(e => Variant(e, Nil)))
      putInRep(context.get,PollCommand.addQuestion(poll, question))
      "Номер добавленного вопроса: " + poll.questions.size

    }.getOrElse("Error : не выбран контекст")
  }

  def deleteQuestion(id:Int, userIDE:User): String = {
    getPoolByIdOption(context.getOrElse(return "Тебе еще рано об этом задумываться")).map { poll =>
      if (!checkRoot(poll, userIDE.id)) return "У тебя здесь нет прав"
      putInRep(context.get, PollCommand.deleteQuestionById(poll,id))
      "Вопрос удален"

    }.getOrElse("Error : не выбран контекст")
  }

  def addAnswerOpen(id:Int, answer:String, user: User): String = {

    context.map(cont => {
      val poll = getPoolByIdOption(cont).get
      if (poll.questions(id).voitedUsers.contains(user)) return "Вы уже голосовали"
      if (poll.questions(id).typeOfQuestion != "open") return "Вы ошиблись методом голосования"
      val b = QuestionHandler.addAnswer(poll.questions(id),poll.anonymity,  0, Answer(answer,Option(user)))
      val a = PollCommand.updateQuestion(poll, id, b)
      putInRep(cont, a)
      "Вы проголосовали"

    }).getOrElse("Нет контекста")
  }


  def addAnswerChoice(id:Int, list: List[Int], user: User): String = {

    context.map(cont => {
      for(i <- list) yield {
        val poll = getPoolByIdOption(cont).get
        if (poll.questions(id).voitedUsers.contains(user)) return "Вы уже голосовали"
        if (poll.questions(id).typeOfQuestion == "choice" && list.size>1) return "Вы ошиблись методом голосования"
        val b = QuestionHandler.addAnswer(poll.questions(id),poll.anonymity,  i, Answer("",Option(user)))
        val a = PollCommand.updateQuestion(poll, id, b)
        putInRep(cont, a)
      }
      "Вы проголосовали"
    }).getOrElse("Нет контекста")

  }


}

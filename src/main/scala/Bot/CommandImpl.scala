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

  def getUserID: Int = {
    userID.next()
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

  def checkRoot(poll: Poll):Boolean = {
    poll.admin == getUserID
  }

  def createPoll(name: String, anonymityVar: Option[String], continuousOrAfterstopVar: Option[String],
                 startTimeVar: Option[String], stopTimeVar: Option[String]): Int = {
    val anonymity = anonymityVar.getOrElse("yes") == "yes"

    val continuousOrAfterstop = continuousOrAfterstopVar.getOrElse("afterstop") == "continuous"

    val startTime1 = startTime(startTimeVar)

    val stopTime1 = stopTime(stopTimeVar)

    val id = getMaxID

    putInRep(id, Poll(name,getUserID, id, anonymity, continuousOrAfterstop, startTime1, stopTime1))


    id
  }

  def listPolls(): String = {
    getRep.aggregate("Current polls: \n")((s, p) => s"$s ${p._1} :   ${p._2.name}\n", _ + _)
  }

  def deletePoll(id: Int): String = {

    getRep.get(id).map { (_) =>
      removeFromRep(id)
      "Poll deleted successfully"
    }.getOrElse("Error: poll is not exist")

  }

  def startPoll(id: Int, date: Date): String = {
    getPoolByIdOption(id).map { poll =>

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


  def stopPoll(id: Int, date: Date): String = {

    getPoolByIdOption(id).map { poll =>

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
    getPoolByIdOption(context.get).map { poll =>
      poll.toString

    }.getOrElse("Error : не выбран контекст")
  }

  def addQuestion(name: String, typeOfQuestion: String, list: List[String]): String = {
    getPoolByIdOption(context.get).map { poll =>

      val question = Question(name,typeOfQuestion, HashSet[User](),list.map(e => Variant(e, Nil)))
      putInRep(context.get,PollCommand.addQuestion(poll, question))
      "Номер добавленного вопроса: " + poll.questions.size

    }.getOrElse("Error : не выбран контекст")
  }

  def deleteQuestion(id:Int): String = {
    getPoolByIdOption(context.get).map { poll =>

      putInRep(context.get, PollCommand.deleteQuestionById(poll,id))
      "Вопрос удален"

    }.getOrElse("Error : не выбран контекст")
  }

  def addAnswerOpen(id:Int, answer:String): String = {
    val l_userId = getUserID

    context.map(cont => {
      val poll = getPoolByIdOption(cont).get
      val b = QuestionHandler.addAnswer(poll.questions(id),poll.anonymity,  0, Answer(answer,Option(User(l_userId))))
      val a = PollCommand.updateQuestion(poll, id, b)
      putInRep(cont, a)
      "Вы проголосовали"

    }).getOrElse("Нет контекста")
  }


  def addAnswerChoice(id:Int, list: List[Int]): String = {

    val l_userId = getUserID

    context.map(cont => {
      for(i <- list) yield {
        val poll = getPoolByIdOption(cont).get
        val b = QuestionHandler.addAnswer(poll.questions(id),poll.anonymity,  i, Answer("",Option(User(l_userId))))
        val a = PollCommand.updateQuestion(poll, id, b)
        putInRep(cont, a)
      }
      "Вы проголосовали"
    }).getOrElse("Нет контекста")

  }


}

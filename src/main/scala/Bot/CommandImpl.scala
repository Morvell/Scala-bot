package Bot

import java.text.SimpleDateFormat
import java.util.Date

import scala.collection.immutable

trait Repository {
  private var polls: Map[Int, Poll] = immutable.Map[Int, Poll]()
  def put(id:Int, poll: Poll) { polls = polls + (id -> poll)}
  def get(): Map[Int, Poll] = polls
  def remove(id:Int){polls = polls - id}
  def clean(){polls = polls.empty}

  def search(id: Int): Poll = {
    polls(id)
  }

  def searchOption(id:Int): Option[Poll] = {
    polls.get(id)
  }
}

object CommandImpl extends Repository {

  private var maxID = 0
  val formatDate = new SimpleDateFormat("hh:mm:ss yy:MM:dd")

  def removeID(){maxID = 0}


  def worker(string: String): Unit = {
    println(string)
  }

  def startTime(time:Option[String]): Date = {

    if (time.isDefined){
      return formatDate.parse(time.getOrElse(formatDate.format(new Date)))
    }
    null
  }

  def stopTime(time:Option[String]): Date = {
    if (time.isDefined){
      return formatDate.parse(time.getOrElse(formatDate.format(new Date)))
    }
    null
  }

  def createPoll(name: String, anonymityVar: Option[String], continuousOrAfterstopVar: Option[String],
                 startTimeVar: Option[String], stopTimeVar: Option[String]): Int = {
    val anonymity = anonymityVar.getOrElse("yes") == "yes"

    val continuousOrAfterstop = continuousOrAfterstopVar.getOrElse("afterstop") == "continuous"

    val startTime1 = startTime(startTimeVar)

    val stopTime1 = stopTime(stopTimeVar)

    val id = maxID

    maxID += 1

    put(id,Poll(name, id, anonymity, continuousOrAfterstop, startTime1, stopTime1))


    id
  }

  def listPolls(): String = {
    get().aggregate("Current polls: \n")((s, p)  => s"$s ${p._1} :   ${p._2.name}\n", _ + _)
  }

  def deletePoll(id: Int): String = {

    get().get(id).map { (_) =>
      remove(id)
      "Poll deleted successfully"
    }.getOrElse("Error: poll is not exist")

  }

  def startPoll(id: Int): String = {
    searchOption(id).map { poll =>

      if(poll.active){
        return "Уже запущен"
      }

      if(poll.start_time == null){
        poll.start()
        return "The poll is started successfully"
      }
      if(poll.start_time.before(new Date())) {
        poll.start()
        return "The poll is started successfully"
      } else { return "Error: еще не время"}
      poll.start()
      return "The poll is started successfully"
    }.getOrElse("Error : poll is not exist")

  }


  def stopPoll(id: Int, now: Date): String = {
     val now = new Date()

    searchOption(id).map { poll =>

      if (!poll.active(now)){
        return "Опрос еще не запущен"
      }
      if(poll.end_time == null){
        poll.stop()
        return "The poll is stopped successfully"
      }
      if(poll.end_time.before(new Date())) {
        poll.stop()
        return "The poll is stopped successfully"
      } else { return "Error: еще не время"}
      poll.stop()
      return "The poll is stopped successfully"

    }.getOrElse("Error: poll is not exist")

  }


  def pollResult(id: Int): String = {
    get().get(id).map { poll =>
      search(id).getResult
    }.getOrElse("Error: poll is not exist")
  }

}

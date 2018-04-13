package Bot

import java.text.SimpleDateFormat
import java.util.Date

import scala.collection.immutable

trait Repository {
  var polls: Map[Int, Poll] = immutable.Map[Int, Poll]() //TODO go to trait repka ACCEPT
}

object CommandImpl extends Repository {

  private var maxID = 0
  val formatDate = new SimpleDateFormat("hh:mm:ss yy:MM:dd")


  def worker(string: String): Unit = {
    println(string)
  }

  def createPoll(name: String, anonymityVar: Option[String], continuousOrAfterstopVar: Option[String],
                 startTimeVar: Option[String], stopTimeVar: Option[String]): Int = {
    val anonymity = anonymityVar.getOrElse("yes") == "yes"
    val continuousOrAfterstop = continuousOrAfterstopVar.getOrElse("afterstop") == "continuous"

    val startTime = formatDate.parse(startTimeVar.getOrElse(formatDate.format(new Date)))
    val stopTime = formatDate.parse(stopTimeVar.getOrElse(formatDate.format(new Date)))

    val id = maxID

    maxID += 1
    polls = polls + (id -> Poll(name, id, anonymity, continuousOrAfterstop, startTime, stopTime))

    id
  }

  def listPolls(): String = {
    "Current polls: " + polls.foreach(x => println(x._1 + ": " + x._2.name)).toString
  }

  def deletePoll(id: Int): String = {

    polls.get(id).map { (_) =>
      polls = polls - id
      "Poll deleted successfully"
    }.getOrElse("Error: poll is not exist")

  }

  def startPoll(id: Int): String = {
    polls.get(id).map { poll =>
      poll.start()
      "The poll is started successfully"
    }.getOrElse("Error : poll is not exist")

  }


  def stopPoll(id: Int): String = {

    polls.get(id).map { poll =>
      poll.stop()
      "The poll is stopped successfully"
    }.getOrElse("Error: poll is not exist")

  }


  def pollResult(id: Int): String = {
    polls.get(id).map { poll =>
      polls(id).getResult
    }.getOrElse("Error: poll is not exist")
  }

}

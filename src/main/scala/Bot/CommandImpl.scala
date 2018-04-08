package Bot

import java.text.SimpleDateFormat
import java.util.Date

import scala.collection.immutable

object CommandImpl {

  private var maxID = 0
  private var polls = immutable.Map[Int, Poll]()  //TODO go to trait repka
  val formatDate = new SimpleDateFormat("hh:mm:ss yy:MM:dd")


  def createPoll(name : String, anonymityVar : Option[String], continuousOrAfterstopVar : Option[String],
                 startTimeVar : Option[String], stopTimeVar : Option[String]): Unit = {
    val anonymity = anonymityVar.getOrElse("yes") == "yes"
    val continuousOrAfterstop = continuousOrAfterstopVar.getOrElse("afterstop") == "continuous"

    val startTime = formatDate.parse(startTimeVar.getOrElse(formatDate.format(new Date())))
    val stopTime = formatDate.parse(stopTimeVar.getOrElse(formatDate.format(new Date())))

    val id = maxID

    maxID += 1
    polls = polls + (id -> new Poll(name, id, anonymity, continuousOrAfterstop, startTime, stopTime))

    println(id)
  }

  def listPolls(): Unit = {
    println("Current polls: ")
    polls.foreach(x => println(x._1 + ": " + x._2.name))
  }

  def deletePoll(id : Int): Unit = { polls.contains(id) match {
    case true => polls = polls - id
                 println("Poll deleted successfully")
    case false =>  println("Error: poll is not exist")
  }

  }

  def startPoll(id : Int): String = {
    polls.get(id).map { poll =>
      poll.start
      "The poll is started successfully"
    }.getOrElse("Error : poll is not exist")

    polls.contains(id) match { //TODO polls.get
    case true => polls(id).start() //TODO time start
      println("The poll is started successfully")
    case false => println("Error : poll is not exist")
  }
  }

  def stopPoll(id : Int): Unit = { polls.contains(id) match {
    case true => polls(id).stop() //TODO make check start
      println("The poll is stopped successfully")
    case false => println("Error: poll is not exist") //TODO remove println
  }
  }


  def pollResult(id : Int): Unit = { polls.contains(id) match {
    case true =>  println(polls(id).getResult)
    case false =>  println("Error: poll is not exist")
  }
  }

}

package Bot

import java.util.Date

case class Poll(name : String, id : Int,
                anonymity : Boolean = true,
                continuousOrAfterstop : Boolean = false,
                start_time : Option[Date] = None,
                end_time : Option[Date] = None,
               )


object PollCommand {

  def active(poll: Poll, date: Date): Boolean = {
    if(poll.start_time.isEmpty) return false

    val t = poll.end_time.getOrElse(new Date())

    val a1 = t.after(date)

    val a2 = poll.start_time.get.before(date)

    a1 && a2
  }

  def start(poll: Poll, date: Date): Poll = {
    if (poll.start_time.isDefined) return poll
    poll.copy(start_time = Option(date))
  }
  def stop(poll: Poll, date: Date): Poll = {
    if (poll.end_time.isDefined) return poll
    poll.copy(end_time = Option(date))
  }
  def getResult(poll: Poll, date: Date) : String = { if (active(poll, date) && !poll.continuousOrAfterstop) {
    "Can not see before finished"
  } else {
    "The poll " + poll.name + " has following result: "
  }
  }
}
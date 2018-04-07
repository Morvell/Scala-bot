package Bot

import java.util.Date

trait PoolTrait {
  val name : String = ""
  val id : Int = 0
  val anonymity : Boolean = true
  val continuousOrAfterstop : Boolean = false
  val start_time : Date = null
  val end_time : Date = null

}

class Poll(name : String, id : Int, anonymity : Boolean = true,
           continuous_visibility : Boolean = false,
           start_time : Date = null,
           end_time : Date = null) extends PoolTrait {

  var active = false
  def start(): Unit = active = true
  def stop(): Unit = active = false
  def getResult : String = { active && !continuousOrAfterstop match {
    case true => "Can not see before finished"
    case _ =>  "The poll" + name + "has following result:"
  }
  }

}
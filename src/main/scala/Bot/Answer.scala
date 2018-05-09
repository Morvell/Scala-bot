package Bot

trait Answer

case class AnswerOpen(list: List[String]) extends Answer
case class AnswerChoice(map: Map[Int,Int]) extends Answer
case class AnswerMulti(map: Map[Int, Int]) extends Answer


package Bot

trait Command

object Matcher {

  case class CreatePoolM (name:String, anon:Option[String], cont:Option[String], start:Option[String], stop:Option[String]) extends Command
  case class ListM () extends Command
  case class DeleteM(d:Int) extends Command
  case class StartM(d:Int) extends Command
  case class StopM(d:Int) extends Command
  case class ResultM(d:Int) extends Command
  case class BeginM(d:Int) extends Command
  case class EndM() extends Command
  case class ViewM() extends Command
  case class DeleteQuestionM(d:Int) extends Command
  case class AnswerM(d:Int, s:String) extends Command
  case class Fail() extends Command{
    def apply(): String = "Bad command"}


}

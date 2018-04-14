package Bot

import scala.util.parsing.combinator.RegexParsers

class CommandParser extends RegexParsers {

  private val command = CommandImpl

  def createPoll: Parser[Unit] = {
    val pollName = Parser("((" ~> """\w+""".r <~"))" | """\w+""".r) //TODO () ACCEPT
    val anonymity = Parser("(" ~> ("yes" | "no") <~ ")")
    val continuous = Parser("(" ~> ("afterstop" | "continuous") <~ ")")
    val startTime = Parser("(" ~> """\d{2}:\d{2}:\d{2} \d{2}:\d{2}:\d{2}""".r <~ ")")
    val stopTime = startTime
    ("/create_poll (" ~> pollName <~ ")") ~ anonymity.? ~ continuous.? ~ startTime.? ~ stopTime.? ^^
      { case name ~ anonym ~ contin ~ start ~ stop  =>  command.worker(command.createPoll( name, anonym, contin,
        start, stop).toString)}  //TODO case ~ ACCEPT
  }

  def list: Parser[Unit] = """^/list""".r ^^ { _ => Matcher.ListM(_) }
  def delete: Parser[Unit] = "/delete_poll (" ~> """\d+""".r <~ ")" ^^ { _ => Matcher.DeleteM(_) } //TODO separate NEED HELP
  def start: Parser[Unit] = "/start_poll (" ~> """\d+""".r <~ ")" ^^ { _ => Matcher.StartM(_) }
  def stop: Parser[Unit] = "/stop_poll (" ~> """\d+""".r <~ ")" ^^ { _ => Matcher.StopM(_) }
  def result: Parser[Unit] = "/result (" ~> """\d+""".r <~ ")" ^^ { _ => Matcher.ResultM(_) }

  def apply(input: String): Unit = parse(
    createPoll | list | delete | start | stop | result, input)
  match {
    case Success(result,_) => result
    case Failure(result, _) => CommandImpl.worker("Bad command: " + input)
  }
}

object CommandParser {
  def apply: CommandParser = new CommandParser()
}
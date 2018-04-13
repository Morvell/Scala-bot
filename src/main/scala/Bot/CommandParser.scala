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
      { case name ~ anonym ~ contin ~ start ~ stop  =>  command.createPoll( name, anonym, contin,
        start, stop)}  //TODO case ~ ACCEPT
  }

  def list: Parser[Unit] = """^/list""".r ^^ { _ => command.listPolls() }
  def delete: Parser[Unit] = "/delete_poll (" ~> """\d+""".r <~ ")" ^^ { d => command.deletePoll(d.toInt) } //TODO separate
  def start: Parser[Unit] = "/start_poll (" ~> """\d+""".r <~ ")" ^^ { d => command.startPoll(d.toInt) }
  def stop: Parser[Unit] = "/stop_poll (" ~> """\d+""".r <~ ")" ^^ { d => command.stopPoll(d.toInt) }
  def result: Parser[Unit] = "/result (" ~> """\d+""".r <~ ")" ^^ { d => command.pollResult(d.toInt) }

  def apply(input: String): Unit = parse(
    createPoll | list | delete | start | stop | result, input)
  match {
    case Success(result,_) => result //TODO experement with Phrase IGNORE
    case Failure(result, _) => println("Bad command: " + input)
  }
}

object CommandParser {
  def apply: CommandParser = new CommandParser()
}
package Bot

import scala.util.parsing.combinator.RegexParsers

class CommandParser extends RegexParsers {

  private val command = CommandImpl

  def createPoll: Parser[Unit] = {
    val pollName = Parser("""\w+""".r)
    val anonymity = Parser("(" ~> ("yes" | "no") <~ ")")
    val continuous = Parser("(" ~> ("afterstop" | "continuous") <~ ")")
    val startTime = Parser("(" ~> """\d{2}:\d{2}:\d{2} \d{2}:\d{2}:\d{2}""".r <~ ")")
    val stopTime = startTime
    ("/create_poll (" ~> pollName <~ ")") ~ anonymity.? ~ continuous.? ~ startTime.? ~ stopTime.? ^^
      { s =>  command.createPoll( s._1._1._1._1, s._1._1._1._2, s._1._1._2,
        s._1._2, s._2)}
  }

  def list: Parser[Unit] = """^/list""".r ^^ { _ => command.listPolls() }
  def delete: Parser[Unit] = "/delete_poll (" ~> """\d+""".r <~ ")" ^^ { d => command.deletePoll(d.toInt) }
  def start: Parser[Unit] = "/start_poll (" ~> """\d+""".r <~ ")" ^^ { d => command.startPoll(d.toInt) }
  def stop: Parser[Unit] = "/stop_poll (" ~> """\d+""".r <~ ")" ^^ { d => command.stopPoll(d.toInt) }
  def result: Parser[Unit] = "/result (" ~> """\d+""".r <~ ")" ^^ { d => command.pollResult(d.toInt) }

  def apply(input: String): Unit = parse(
    createPoll | list | delete | start | stop | result, input)
  match {
    case Success(result, _) => result
    case _ : NoSuccess => println("No success parse")
  }
}

object CommandParser {
  def apply: CommandParser = new CommandParser()
}
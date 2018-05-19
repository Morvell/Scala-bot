package Bot

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class CommandParser extends RegexParsers {
  override protected val whiteSpace: Regex = """ +""".r
  private val command = CommandImpl

  def createPoll: Parser[Command] = {
    val pollName = Parser("((" ~> """\w+""".r <~ "))" | """\w+""".r) //TODO () ACCEPT
    val anonymity = Parser("(" ~> ("yes" | "no") <~ ")")
    val continuous = Parser("(" ~> ("afterstop" | "continuous") <~ ")")
    val startTime = Parser("(" ~> """\d{2}:\d{2}:\d{2} \d{2}:\d{2}:\d{2}""".r <~ ")")
    val stopTime = startTime

    ("/create_poll" ~> "(" ~> pollName <~ ")") ~ anonymity.? ~ continuous.? ~ startTime.? ~ stopTime.? ^^
      { case name ~ anon ~ cont ~ start ~ stop => Matcher.CreatePollM(name, anon, cont, start, stop) } //TODO case ~ ACCEPT
  }

  def list: Parser[Command] = """^/list""".r ^^ { _ => Matcher.ListM() }

  def delete: Parser[Command] =
    "/delete_poll" ~> "(" ~>"""\d+""".r <~ ")" ^^ { d => Matcher.DeleteM(d.toInt) } //TODO separate ACCEPT
  def start: Parser[Command] = "/start_poll" ~> "(" ~> """\d+""".r <~ ")" ^^ { d => Matcher.StartM(d.toInt) }

  def help: Parser[Command] = "/help" ^^ { _ => Matcher.PrintHelpM() }

  def stop: Parser[Command] = "/stop_poll" ~> "(" ~> """\d+""".r <~ ")" ^^ { d => Matcher.StopM(d.toInt) }

  def result: Parser[Command] = "/result" ~> "(" ~> """\d+""".r <~ ")" ^^ { d => Matcher.ResultM(d.toInt) }

  def begin: Parser[Command] = "/begin" ~> "(" ~> """\d+""".r <~ ")" ^^ { d => Matcher.BeginM(d.toInt) }

  def end: Parser[Command] = "/end" ^^ { _ => Matcher.EndM() }

  def view: Parser[Command] = "/view" ^^ { _ => Matcher.ViewM() }

  def deleteQuestion: Parser[Command] =
    "/delete_question" ~> "(" ~> """\d+""".r <~ ")" ^^ { d => Matcher.DeleteQuestionM(d.toInt) }

  def addQuestionChoice: Parser[Command] = {
    val question = Parser("(" ~> """[_a-zA-Zа-яА-ЯёЁ0-9.,:;'"*&!? ]+""".r <~ ")")
    val questionType = Parser("(" ~> ("choice"|"multi") <~ ")")
    val variant = Parser("\n" ~> """[_a-zA-Zа-яА-ЯёЁ0-9.,:;'"*&!? ]+""".r )
    val command = Parser("/add_question" ~> question)
    command~questionType~rep(variant) ^^ { case a~b~c => Matcher.AddQuestionChoiceM(a, b, c)}
  }

  def addQuestionOpen: Parser[Command] = {
    val question = Parser("(" ~> """[_a-zA-Zа-яА-ЯёЁ0-9.,:;'"*&!? ]+""".r <~ ")")
    val questionType = Parser("(" ~> "open" <~ ")")
    val command = Parser("/add_question" ~> question)
    command~questionType^^ { case a~b => Matcher.AddQuestionOpenM(a, b)}
  }

  def answerOpen: Parser[Command] = {
    val digit = Parser("(" ~> """\d+""".r <~ ")")
    val otvet = Parser("(" ~> """[_a-zA-Zа-яА-ЯёЁ0-9.,:;'"*&!? ]+""".r <~ ")")
    val command = Parser("/answer" ~> digit)
    command~otvet ^^ { case a~b => Matcher.AnswerStringM(a.toInt, b)}
  }

  def answerChoice: Parser[Command] = {
    val digit = Parser("(" ~> """\d+""".r <~ ")")
    val command = Parser("/answer" ~> digit)
    command~digit ^^ { case a~b => Matcher.AnswerDigitM(a.toInt, b.toInt::Nil)}
  }

  def answerMulti: Parser[Command] = {
    val digit = Parser("(" ~> """\d+""".r <~ ")")
    val variant = Parser("\n" ~> """\d+""".r )
    val command = Parser("/answer" ~> digit)
    command~rep(variant) ^^ { case a~b => Matcher.AnswerDigitM(a.toInt, b.map(_.toInt))}
  }

  val rlt: Parser[Command] = createPoll | list | delete | start | stop | result |
    begin | end |view | answerOpen | deleteQuestion | addQuestionChoice | addQuestionOpen |
    answerChoice | answerMulti | help | failure(s"Oops, I don't know this command 🤔. Try typing /help")


  def apply(input: String): ParseResult[Command] = parse(rlt, input)
}

object CommandParser extends CommandParser
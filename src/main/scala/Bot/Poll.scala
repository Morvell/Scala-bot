package Bot

import java.util.Date

case class Poll(name: String,
                id: Int,
                admin: Long,
                anonymity: Boolean = true,
                continuousOrAfterstop: Boolean = false,
                start_time: Option[Date] = None,
                end_time: Option[Date] = None,
                questions: List[Question] = List(),
               )

object PollCommand {
  def addQuestion(poll: Poll, q:Question): Poll = poll.copy(questions = poll.questions :+ q)

  def updateQuestion(poll: Poll, id:Int, q:Question): Poll =
    poll.copy(questions = poll.questions.updated(id, q))

  def deleteQuestion(poll: Poll, q:Question) : Poll =
    poll.copy(questions = poll.questions.filter(_ != q))

  def deleteQuestionById(poll: Poll, id:Int) : Poll =
    deleteQuestion(poll, poll.questions(id))

  def pollCanNotBeStarted(poll: Poll, date: Date): Boolean =
    poll.end_time.exists(d => d.before(date))

  def active(poll: Poll, date: Date = new Date()): Boolean =
    if (poll.start_time.isEmpty) false
    else {
      val t = poll.end_time.getOrElse(new Date())
      t.compareTo(date) >= 0 && poll.start_time.get.before(date)
    }

  def start(poll: Poll, date: Date): Poll =
    if (poll.start_time.isDefined) poll
    else poll.copy(start_time = Option(date))

  def stop(poll: Poll, date: Date): Poll =
    if (poll.end_time.isDefined) poll
    else poll.copy(end_time = Option(date))

  def getView(poll: Poll) : String = {
    val result = for ((q, i) <- poll.questions.zipWithIndex) yield {
      q.typeOfQuestion match {
        case "multi" => s"*$i)* ${q.name} _(multi)_:\n${getAggregatedOptions(q)}"
        case "choice" => s"*$i)* ${q.name} _(choice)_:\n${getAggregatedOptions(q)}"
        case "open" => s"*$i)* ${q.name} _(open)_"
      }
    }
    s"👀 Poll *'${poll.name}'* has ${poll.questions.size} questions:\n${result.mkString("\n")}"
  }

  def getAggregatedOptions(question: Question): String = {
    question.variants.map(x => x.name).mkString("\n")
  }

  def getResult(poll: Poll, date: Date) : String = {
    if (active(poll, date) && !poll.continuousOrAfterstop) {
      s"Poll results are hidden until _${poll.end_time.getOrElse(new Date())}_ ⏰"
  } else {
      val result = for (q <- poll.questions) yield {
        val questionResult = if (poll.anonymity) {
          q.typeOfQuestion match {
            case "multi" => getAnonChoiceResult(q)
            case "choice" => getAnonChoiceResult(q)
            case "open" => getAnonOpenResult(q)
          }
        } else {
          q.typeOfQuestion match {
            case "multi" => getNonAnonChoiceResult(q)
            case "choice" => getNonAnonChoiceResult(q)
            case "open" => getNonAnonOpenResult(q)
          }
        }
        getQuestionInfo(q) + questionResult
      }
      s"👀 Poll *'${poll.name}'* results:\n${result.mkString("\n")}"
    }
  }

  def getQuestionInfo(question: Question): String = {
    s"👉 `${question.name}` 👈, voted ${question.votedUsers.size} people:"
  }

  def getAnonOpenResult(question: Question): String = {
    question.variants.head.answers.aggregate("\n")((c,ans) =>  c + s"⋅    ${ans.answer}\n", _ + _)
  }

  def getNonAnonOpenResult(question: Question): String = {
    question.variants.head.answers.aggregate("\n")((c,ans) =>  c + s"⋅    ${ans.user.get.name}: ${ans.answer}\n", _ + _)
  }

  def getAnonChoiceResult(question: Question): String = {
    question.variants.aggregate("\n")((b,variant) => b + s"⋅    ${variant.name}: _${variant.answers.size}_\n", _ + _)
  }

  def getNonAnonChoiceResult(question: Question): String = {
    question.variants.aggregate("\n")((b,variant) => b + s"⋅    ${variant.name}: _${variant.answers.size}_\n" +
      variant.answers.aggregate("")((k, ans) => k + s"⁘      _(${ans.user.get.name})_", _ + _) + "\n", _ + _)
  }
}
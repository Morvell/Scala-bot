package Bot

import java.util.Date

case class Poll(name : String, id : Int,
                admin : Long,
                anonymity : Boolean = true,
                continuousOrAfterstop : Boolean = false,
                start_time : Option[Date] = None,
                end_time : Option[Date] = None,
                questions : List[Question] = List(),
               )


object PollCommand {

  def addQuestion(poll: Poll, q:Question): Poll = {
    poll.copy(questions = poll.questions :+ q)
  }

  def updateQuestion(poll: Poll, id:Int, q:Question): Poll = {
    poll.copy(questions = poll.questions.updated(id,q))
  }

  def deleteQuestion(poll: Poll, q:Question) : Poll = {
    poll.copy(questions = poll.questions.filter(_ != q))
  }

  def deleteQuestionById(poll: Poll, id:Int) : Poll = {
    deleteQuestion(poll, poll.questions(id))
  }

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

  def getView(poll: Poll) : String = {


    val result = for (q <- poll.questions) yield {
      q.typeOfQuestion match {
        case "multi" => getViewChoiceResult(q)
        case "choice" => getViewChoiceResult(q)
        case "open" => getViewOpenResult(q)
      }

    }
    result.aggregate("")((b, str) => b + str + "\n", _ + _)


  }

  def getViewOpenResult(question: Question): String = {
    question.name +" open голосование" + "\n"
  }

  def getViewChoiceResult(question: Question): String = {
    val id = Stream.from(0).iterator
    question.name +" " + question.typeOfQuestion + " голосование" + question.variants.aggregate("\n")((b,variant) => b+"  "+id.next() + ") "+variant.name+"\n", _ + _)

  }



  def getResult(poll: Poll, date: Date) : String = {
    if (active(poll, date) && !poll.continuousOrAfterstop) {
      "Can not see before finished"
  } else {
    poll.anonymity match {
      case true => {
        val result = for (q <- poll.questions) yield {
          q.typeOfQuestion match {
            case "multi" => getAnonChoiceResult(q)
            case "choice" => getAnonChoiceResult(q)
            case "open" => getAnonOpenResult(q)
          }

        }
        result.aggregate("")((b,str) => b + str +"\n",_ + _)
      }
      case false => {
        val result = for (q <- poll.questions) yield {
          q.typeOfQuestion match {
            case "multi" => getNonAnonChoiceResult(q)
            case "choice" => getNonAnonChoiceResult(q)
            case "open" => getNonAnonOpenResult(q)
          }
      }
        result.aggregate("")((b,str) => b + str +"\n",_ + _)
  }}}
  }

  def getAnonOpenResult(question: Question): String = {
    question.name + "\n" + "Проголосовало: " + question.voitedUsers.size + question.variants.head.answers.aggregate("\n")((c,ans) =>  c + "# " + ans.answer + "\n", _ + _)
//    question.aggregate("\n")((a,b) => a + b.name + "\n" + "Проголосовало: " + b.voitedUsers.size + "\n" + b.variants.head.answers.aggregate("\n")((c,ans) =>  c + "# " + ans.answer + "\n", _ + _) + "\n", _ + _)
  }

  def getNonAnonOpenResult(question: Question): String = {

    question.name + "\n" + "Проголосовало: " + question.voitedUsers.size + question.variants.head.answers.aggregate("\n")((c,ans) =>  c + "# "+ans.user.get.name +"---" + ans.answer + "\n", _ + _)

  }

//  def makeHistogram(question: Question): String = {
//    val maxLen = 15
//    question.
//
//  }

  def getAnonChoiceResult(question: Question): String = {
    question.name + "\n" + "Проголосовало: " + question.voitedUsers.size + question.variants.aggregate("\n")((b,variant) => b+"  " +variant.name+": " + variant.answers.size + "\n", _ + _)

  }

  def getNonAnonChoiceResult(question: Question): String = {
    question.name + "\n" + "Проголосовало: " + question.voitedUsers.size + question.variants.aggregate("\n")((b,variant) => b+"  " + variant.name+": " + variant.answers.size + "\n" + "Голосовали: " + variant.answers.aggregate("")((k, ans) => k + ans.user.get.name + ",", _ + _) + "\n", _ + _)

  }
}
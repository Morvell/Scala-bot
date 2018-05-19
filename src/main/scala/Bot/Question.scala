package Bot

import scala.collection.immutable.HashSet

case class Question(name: String, typeOfQuestion: String, votedUsers:HashSet[User], variants: List[Variant])
case class User(id:Long, name:String)
case class Answer(answer: String, user: Option[User])
case class Variant(name: String, answers: List[Answer])

object QuestionHandler {

  def rename(question: Question, newName:String):Question = {
    question.copy(name = newName)
  }

  def changeTypeOfQuestion(question: Question, newType:String) : Question = {
    question.copy(typeOfQuestion = newType)
  }

  def addAnswer(question: Question, anonymity:Boolean, id:Int, answer: Answer): Question = {
    if (anonymity) {
      val a = question.variants(id).copy(answers = answer.copy(user = None) :: question.variants(id).answers)
      question.copy(variants = question.variants.updated(id, a)).copy(votedUsers = question.votedUsers + answer.user.get)
    } else {
      val a = question.variants(id).copy(answers = answer :: question.variants(id).answers)
      question.copy(variants = question.variants.updated(id, a)).copy(votedUsers = question.votedUsers + answer.user.get)
    }

  }

  def addAnswerMulti(question: Question, anonymity: Boolean, id: Int, answer: Answer): Question =
    if (anonymity) {
      val a = question.variants(id).copy(answers = answer.copy(user = None) :: question.variants(id).answers)
      question.copy(variants = question.variants.updated(id, a))
    } else {
      val a = question.variants(id).copy(answers = answer :: question.variants(id).answers)
      question.copy(variants = question.variants.updated(id, a))
    }

  def addVotedUser(question: Question, answer: Answer): Question = {
    question.copy(votedUsers = question.votedUsers + answer.user.get)
  }

}
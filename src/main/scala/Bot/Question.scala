package Bot

import scala.collection.immutable.HashSet


case class Question(name: String, typeOfQuestion: String, voitedUsers:HashSet[User], variants: List[Variant])

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
    anonymity match {
      case false => {
        val a = question.variants(id).copy(answers = answer :: question.variants(id).answers)
        question.copy(variants = question.variants.updated(id, a)).copy(voitedUsers = question.voitedUsers + answer.user.get)

      }
      case true => {
        val a = question.variants(id).copy(answers = answer.copy(user = None) :: question.variants(id).answers)
        question.copy(variants = question.variants.updated(id, a)).copy(voitedUsers = question.voitedUsers + answer.user.get)
      }
    }

  }

  def addAnswerMulti(question: Question, anonymity: Boolean, id: Int, answer: Answer): Question = {
    anonymity match {
      case false => {
        val a = question.variants(id).copy(answers = answer :: question.variants(id).answers)
        question.copy(variants = question.variants.updated(id, a))

      }
      case true => {
        val a = question.variants(id).copy(answers = answer.copy(user = None) :: question.variants(id).answers)
        question.copy(variants = question.variants.updated(id, a))
      }
    }

  }

  def addInVoitedUser(question: Question, answer: Answer): Question = {
    question.copy(voitedUsers = question.voitedUsers + answer.user.get)
  }

}
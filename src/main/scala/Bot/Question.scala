package Bot


case class Question(name: String, typeOfQuestion: String, variants: List[Variant])

case class User(id:Int,answer:String)

case class Variant(name: String, answers: List[User])

object QuestionHandler {

  def rename(question: Question, newName:String):Question = {
    question.copy(name = newName)
  }

  def changeTypeOfQuestion(question: Question, newType:String) : Question = {
    question.copy(typeOfQuestion = newType)
  }

  def addAnswer(question: Question, id:Int, answer: User): Question = {
    question.copy(variants = question.variants.updated(id, question.variants(id).copy(answers = answer :: question.variants(id).answers)))
  }

}
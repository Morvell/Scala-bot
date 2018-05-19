package Bot

import org.scalatest._

class CommandImplTest extends FlatSpec with Matchers with BeforeAndAfterEach {

  "CreatePool" should "return id" in {
    val created = CommandImpl.createPoll("test", Option("yes"), Option("test"),
      Option("13:22:00 18:03:26"), Option("13:22:00 18:03:26"))
    val (id, _) = CommandImpl.polls.head
    id shouldBe 0
    created shouldBe Answers.pollWasCreated(0, "test")
  }

  "CreatePool" should "return  unic id" in {
    val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:03:26"),Option("13:22:00 18:03:26"))
    val r2 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:03:26"),Option("13:22:00 18:03:26"))
    r1 should not be r2
  }

  "CreatePool" should "return  unic id #2" in {
    val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:03:26"),Option("13:22:00 18:03:26"))
    val r2 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:03:26"),Option("13:22:00 18:03:26"))
    val r3 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:03:26"),Option("13:22:00 18:03:26"))
    CommandImpl.removePoll(2)
    val r4 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:03:26"),Option("13:22:00 18:03:26"))
    r1 should not be r2
    r2 should not be r3
    r3 should not be r4
  }

  "CreatePool" should "correct Pool" in {
    CommandImpl.createPoll("test",Option("yes"),Option("test"),Option(null),Option(null))
    val (_, poll) = CommandImpl.polls.head
    poll.name shouldBe "test"
    PollCommand.active(poll, CommandImpl.formatDate.parse("13:22:00 18:03:27") ) shouldBe false
    poll.anonymity shouldBe true
    poll.start_time shouldBe None
  }

  "CreatePool" should "correct Pool #2" in {
    CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:03:26"),Option("13:22:00 18:03:26"))
    val (_, poll) = CommandImpl.polls.head
    poll.name shouldBe "test"
    PollCommand.active(poll, CommandImpl.getTimeFromFormat("13:22:00 18:03:26")) shouldBe false
    poll.anonymity shouldBe true
    poll.start_time.get shouldBe CommandImpl.formatDate.parse("13:22:00 18:03:26")
    poll.end_time.get shouldBe CommandImpl.formatDate.parse("13:22:00 18:03:26")
  }


  "StartPoll" should "correct start" in {
    CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:03:26"),Option("13:22:00 18:03:27"))
    val (_, poll) = CommandImpl.polls.head
    CommandImpl.startPoll(poll.id, CommandImpl.getTimeFromFormat("13:22:01 18:03:26"),
      User(poll.admin, "")) shouldBe Answers.pollIsRunning
    PollCommand.active(poll,CommandImpl.getTimeFromFormat("13:22:01 18:03:26")) shouldBe true
  }

  "StartPoll" should "correct start without time" in {
    CommandImpl.createPoll("test",Option("yes"),Option("test"),Option(null),Option("13:22:00 18:03:26"))
    val (_, poll) = CommandImpl.polls.head
    CommandImpl.startPoll(poll.id, CommandImpl.getTimeFromFormat("13:22:01 18:03:25"),
      User(poll.admin, "")) shouldBe Answers.pollWasStarted
    val (_, updatedPoll) = CommandImpl.polls.head
    PollCommand.active(updatedPoll, CommandImpl.getTimeFromFormat("13:22:02 18:03:25")) shouldBe true
  }

  "StartPoll" should "alredy start" in {
    CommandImpl.createPoll("test",Option("yes"),Option("test"),None,Option("13:22:00 18:03:26"))
    val (_, poll) = CommandImpl.polls.head
    CommandImpl.startPoll(poll.id, CommandImpl.getTimeFromFormat("13:22:00 18:02:16"), User(poll.admin,""))
    CommandImpl.startPoll(poll.id, CommandImpl.getTimeFromFormat("13:22:01 18:02:16"),
      User(poll.admin, "")) shouldBe Answers.pollIsRunning
  }

  "StartPoll" should "correct don't start " in {
    CommandImpl.createPoll("test", Option("yes"), Option("test"), Option("13:22:00 18:02:16"), Option("13:22:00 18:03:26"))
    val (_, poll) = CommandImpl.polls.head
    CommandImpl.startPoll(poll.id, CommandImpl.getTimeFromFormat("13:22:01 18:02:17"),
      User(poll.admin, "")) shouldBe Answers.pollIsRunning
    PollCommand.active(poll,CommandImpl.getTimeFromFormat("13:22:02 18:03:25")) shouldBe true
  }

  "StartPoll" should "Success start without start time" in {
    CommandImpl.createPoll("test", Option("yes"), Option("test"), None, Option("13:22:00 18:03:26"))
    val (_, poll) = CommandImpl.polls.head
    CommandImpl.startPoll(poll.id, CommandImpl.getTimeFromFormat("13:22:01 18:02:17"),
      User(poll.admin, "")) shouldBe Answers.pollWasStarted
    val (_, updatedPoll) = CommandImpl.polls.head
    PollCommand.active(updatedPoll, CommandImpl.getTimeFromFormat("13:22:02 18:03:25")) shouldBe true
  }

  "StartPoll" should  "Error : poll is not exist" in {
    CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:99"),Option("13:22:00 18:03:26"))
    val (_, poll) = CommandImpl.polls.head
    CommandImpl.startPoll(99, CommandImpl.getTimeFromFormat("13:22:02 18:03:25"),
      User(poll.admin, "")) shouldBe Answers.noSuchPoll
  }

  "StopPoll" should  "Succes stop with time" in {
    CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:18"),Option("13:22:00 18:03:26"))
    val (_, poll) = CommandImpl.polls.head
    CommandImpl.stopPoll(poll.id, CommandImpl.getTimeFromFormat("13:22:02 18:03:25"),
      User(poll.admin, "")) shouldBe Answers.pollWillStartAutomatically
    PollCommand.active(poll,CommandImpl.getTimeFromFormat("13:22:02 18:03:27")) shouldBe false
  }

  "StopPoll" should  "Succes stop witout time" in {
    CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:18"),Option(null))
    val (_, poll) = CommandImpl.polls.head
    CommandImpl.stopPoll(poll.id, CommandImpl.getTimeFromFormat("13:22:02 18:03:25"),
      User(poll.admin, "")) shouldBe Answers.pollIsStopped
    val (_, updatedPoll) = CommandImpl.polls.head
    PollCommand.active(updatedPoll, CommandImpl.getTimeFromFormat("13:22:02 18:03:27")) shouldBe false
  }

  "StopPoll" should  "Unsucces active witout time" in {
    CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:18"),Option(null))
    val (_, poll) = CommandImpl.polls.head
    PollCommand.active(poll,CommandImpl.getTimeFromFormat("13:22:02 18:03:27")) shouldBe true
  }
  
  "StopPoll" should  "Error: poll is not exist" in {
    CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:18"),Option("13:22:00 18:03:99"))
    val (_, poll) = CommandImpl.polls.head
    CommandImpl.stopPoll(99, CommandImpl.getTimeFromFormat("13:22:02 18:03:27"),
      User(poll.admin, "")) shouldBe Answers.noSuchPoll
  }

  "DeletePoll" should  "Success delete" in {
    CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:18"),Option("13:22:00 18:03:99"))
    val (_, poll) = CommandImpl.polls.head
    CommandImpl.deletePoll(poll.id, User(poll.admin, "")) shouldBe Answers.pollWasDeleted
    CommandImpl.polls.isEmpty shouldBe true
  }

  "/view" should "x3" in {
    CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:18"),Option("13:22:00 18:03:99"))
    val (_, poll) = CommandImpl.polls.head
    CommandImpl.setContext(poll.admin, poll.id)
    CommandImpl.addQuestion("Тестовый вопрос?", "multi","1"::"2"::Nil, User(poll.admin,""))
    print(CommandImpl.view(User(poll.admin, "")))
  }


  "AddQuestion" should "simple" in {
    CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:18"),Option("13:22:00 18:03:99"))
    val (_, poll) = CommandImpl.polls.head
    CommandImpl.setContext(poll.admin, poll.id)
    CommandImpl.addQuestion("Тестовый вопрос?", "multi","1"::"2"::Nil, User(poll.admin,""))
    val (_, updatedPoll) = CommandImpl.polls.head
    updatedPoll.questions.head.name shouldBe "Тестовый вопрос?"
    updatedPoll.questions.head.variants.size shouldBe 2
  }

  "AddQuestion" should "multi" in {
    CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:18"),Option("13:22:00 18:03:99"))
    val (_, poll) = CommandImpl.polls.head
    CommandImpl.setContext(poll.admin, poll.id)
    CommandImpl.addQuestion("Тестовый вопрос?", "multi","1"::"2"::Nil, User(poll.admin,""))
    CommandImpl.addQuestion("Тестовый вопрос2?", "multi","1"::Nil, User(poll.admin,""))
    val (_, updatedPoll) = CommandImpl.polls.head

    updatedPoll.questions.head.name shouldBe "Тестовый вопрос?"
    updatedPoll.questions.head.variants.size shouldBe 2
    updatedPoll.questions(1).name shouldBe "Тестовый вопрос2?"
    updatedPoll.questions(1).variants.size shouldBe 1
  }

  "AddAnswer" should "simple" in {
    CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:18"),Option("13:22:00 18:03:99"))
    val (_, poll) = CommandImpl.polls.head
    CommandImpl.setContext(poll.admin, poll.id)
    CommandImpl.addQuestion("Тестовый вопрос?", "multi","1"::"2"::"3"::Nil, User(poll.admin,""))
    CommandImpl.addAnswerChoice(0, 0::1::Nil,User(poll.admin,""))
    val (_, updatedPoll) = CommandImpl.polls.head

    updatedPoll.questions.size shouldBe 1
    updatedPoll.questions.head.variants.size shouldBe 3
    updatedPoll.questions.head.variants.head.answers.size shouldBe 1
  }

  "AddAnswer" should "simple multi" in {
    CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:18"),Option("13:22:00 18:03:99"))
    val (_, poll) = CommandImpl.polls.head
    CommandImpl.setContext(poll.admin, poll.id)
    CommandImpl.addQuestion("Тестовый вопрос?", "multi","1"::"2"::"3"::Nil, User(poll.admin,""))
    CommandImpl.addAnswerChoice(0, 0::1::Nil,User(poll.admin,"")) shouldBe Answers.thanksForVoting
    val (_, x) = CommandImpl.polls.head
    CommandImpl.addAnswerChoice(0, 0::2::Nil,User(poll.admin,"")) shouldBe Answers.cantVoteTwice
    val (_, updatedPoll) = CommandImpl.polls.head

    updatedPoll.questions.size shouldBe 1
    updatedPoll.questions.head.variants.size shouldBe 3
    updatedPoll.questions.head.variants(2).answers.size shouldBe 0
    updatedPoll.questions.head.variants(1).answers.size shouldBe 1
    updatedPoll.questions.head.variants.head.answers.size shouldBe 1
  }

  "AddAnswer" should "simple with one" in {
    CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:18"),Option("13:22:00 18:03:99"))
    val (_, poll) = CommandImpl.polls.head
    CommandImpl.setContext(poll.admin, poll.id)
    CommandImpl.addQuestion("Тестовый вопрос?", "multi","1"::"2"::"3"::Nil,
      User(poll.admin,"")) shouldBe Answers.questionWasAdded("Тестовый вопрос?", 0)
    CommandImpl.addAnswerChoice(0, 1::Nil, User(poll.admin,"")) shouldBe Answers.thanksForVoting
    val (_, updatedPoll) = CommandImpl.polls.head
    val variants = updatedPoll.questions.head.variants

    updatedPoll.questions.size shouldBe 1
    variants.size shouldBe 3
    variants.head.answers.size shouldBe 0
    variants(2).answers.size shouldBe 0
    variants(1).answers.size shouldBe 1
  }

  "AddAnswerOpen" should "simple" in {
    CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:18"),Option("13:22:00 18:03:99"))
    val (_, poll) = CommandImpl.polls.head
    CommandImpl.setContext(poll.admin, poll.id)
    CommandImpl.setContext(1, poll.id)
    CommandImpl.setContext(2, poll.id)
    CommandImpl.addQuestion("Тестовый вопрос?", "open", "open"::Nil, User(poll.admin,""))
    CommandImpl.addAnswerOpen(0, "Можно",User(1,""))
    CommandImpl.addAnswerOpen(0, "Нельзя",User(2,""))
    val (_, updatedPoll) = CommandImpl.polls.head

    updatedPoll.questions.size shouldBe 1
    updatedPoll.questions.head.variants.size shouldBe 1
    updatedPoll.questions.head.variants.head.answers.size shouldBe 2
  }

  "/result" should "Anon open" in {
    CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:18"),Option("13:22:00 18:03:18"))
    val (_, poll) = CommandImpl.polls.head
    CommandImpl.setContext(poll.admin, poll.id)
    CommandImpl.setContext(1, poll.id)
    CommandImpl.addQuestion("Тестовый вопрос?", "open", "open"::Nil, User(poll.admin,""))
    CommandImpl.addAnswerOpen(0, "Можно",User(poll.admin,""))
    CommandImpl.addAnswerOpen(0, "Нельзя",User(1,""))
    CommandImpl.addQuestion("Тестовый вопрос2?", "open", "open"::Nil, User(poll.admin,""))
    CommandImpl.addAnswerOpen(1, "Можно Наерное",User(poll.admin,""))
    CommandImpl.addAnswerOpen(1, "Нельзя Наверное",User(1,""))
  }

  "/result" should "NoneAnon open" in {
    CommandImpl.createPoll("test",Option("no"),Option("test"),Option("13:22:00 18:02:18"),Option("13:22:00 18:03:18"))
    val (_, poll) = CommandImpl.polls.head
    CommandImpl.setContext(poll.admin, poll.id)
    CommandImpl.setContext(1, poll.id)
    CommandImpl.addQuestion("Тестовый вопрос?", "open", "open"::Nil, User(poll.admin,""))
    CommandImpl.addAnswerOpen(0, "Можно",User(poll.admin,""))
    CommandImpl.addAnswerOpen(0, "Нельзя",User(1,""))
  }

  "/result" should "choice anon" in {
    CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:18"),Option("13:22:00 18:03:99"))
    val (_, poll) = CommandImpl.polls.head
    CommandImpl.setContext(poll.admin, poll.id)
    CommandImpl.setContext(1, poll.id)
    CommandImpl.addQuestion("Тестовый вопрос?", "multi","1"::"2"::"3"::Nil, User(poll.admin,""))
    CommandImpl.addAnswerChoice(0, 0::1::Nil,User(poll.admin,""))
    CommandImpl.addAnswerChoice(0, 0::2::Nil,User(1,""))
    val (_, updatedPoll) = CommandImpl.polls.head
    val expected = "\n⋅    1: _2_\n⋅    2: _0_\n⋅    3: _0_\n"
    PollCommand.getAnonChoiceResult(updatedPoll.questions.head) shouldBe expected
  }

  "/result" should "choice None Anon" in {
    CommandImpl.createPoll("test",Option("no"),Option("test"),Option("13:22:00 18:02:18"),Option("13:22:00 18:03:99"))
    val (_, poll) = CommandImpl.polls.head
    CommandImpl.setContext(poll.admin, poll.id)
    CommandImpl.setContext(1, poll.id)
    CommandImpl.addQuestion("Тестовый вопрос?", "multi","1"::"2"::"3"::Nil, User(poll.admin,""))
    CommandImpl.addAnswerChoice(0, 0::1::Nil,User(poll.admin,"Царь"))
    CommandImpl.addAnswerChoice(0, 0::2::Nil,User(1,"Царь"))
    val (_, updatedPoll) = CommandImpl.polls.head
    val expected = "\n⋅    1: _2_\n⁘      _(Царь)_⁘      _(Царь)_\n⋅    2: _0_\n\n⋅    3: _0_\n\n"
    PollCommand.getNonAnonChoiceResult(updatedPoll.questions.head) shouldBe expected
  }

  "/result" should "full Anon" in {
    CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:15"),Option("13:22:00 18:03:18"))
    val (_, poll) = CommandImpl.polls.head
    CommandImpl.setContext(poll.admin, poll.id)
    CommandImpl.setContext(1, poll.id)
    CommandImpl.addQuestion("Тестовый вопрос?", "multi","1"::"2"::"3"::Nil, User(poll.admin,""))
    CommandImpl.addAnswerChoice(0, 0::1::Nil,User(poll.admin,"Царь"))
    CommandImpl.addAnswerChoice(0, 0::2::Nil,User(1,"Царь2"))
    CommandImpl.addQuestion("Тестовый вопрос2?", "open", "open"::Nil, User(poll.admin,""))
    CommandImpl.addAnswerOpen(1, "Можно",User(poll.admin,"Царь"))
    CommandImpl.addAnswerOpen(1, "Нельзя",User(1,"Царь2"))
    val expected = "\uD83D\uDC40 Poll *'test'* results:\n\uD83D\uDC49 `Тестовый вопрос?` \uD83D\uDC48, voted 2 people:\n⋅    1: _2_\n⋅    2: _0_\n⋅    3: _0_\n\n\uD83D\uDC49 `Тестовый вопрос2?` \uD83D\uDC48, voted 2 people:\n⋅    Нельзя\n⋅    Можно\n"
    PollCommand.getResult(poll,CommandImpl.getTimeFromFormat("13:22:01 26:02:26")) shouldBe expected
  }

  "/result" should "full" in {
    CommandImpl.createPoll("test",Option("no"),Option("test"),Option("13:22:00 18:02:15"),Option("13:22:00 18:03:18"))
    val (_, poll) = CommandImpl.polls.head
    CommandImpl.setContext(poll.admin, poll.id)
    CommandImpl.setContext(1, poll.id)
    CommandImpl.addQuestion("Тестовый вопрос?", "multi","1"::"2"::"3"::Nil, User(poll.admin,""))
    CommandImpl.addAnswerChoice(0, 0::1::Nil,User(poll.admin,"Царь"))
    CommandImpl.addAnswerChoice(0, 0::2::Nil,User(1,"Царь2"))
    CommandImpl.addQuestion("Тестовый вопрос2?", "open", "open"::Nil, User(poll.admin,""))
    CommandImpl.addAnswerOpen(1, "Можно",User(poll.admin,"Царь"))
    CommandImpl.addAnswerOpen(1, "Нельзя",User(1,"2"))
    val expected = "\uD83D\uDC40 Poll *'test'* results:\n\uD83D\uDC49 `Тестовый вопрос?` \uD83D\uDC48, voted 2 people:\n⋅    1: _2_\n⁘      _(Царь2)_⁘      _(Царь)_\n⋅    2: _0_\n\n⋅    3: _0_\n\n\n\uD83D\uDC49 `Тестовый вопрос2?` \uD83D\uDC48, voted 2 people:\n⋅    2: Нельзя\n⋅    Царь: Можно\n"

    PollCommand.getResult(poll,CommandImpl.getTimeFromFormat("13:22:01 26:02:26")) shouldBe expected
  }

  "/view" should "full" in {
    CommandImpl.createPoll("test",Option("no"),Option("test"),Option("13:22:00 18:02:15"),Option("13:22:00 18:03:18"))
    val (_, poll) = CommandImpl.polls.head
    CommandImpl.setContext(poll.admin, poll.id)
    CommandImpl.addQuestion("Тестовый вопрос?", "multi","1"::"2"::"3"::Nil, User(poll.admin,""))
    CommandImpl.addQuestion("Тестовый вопрос?", "multi","5"::"6"::"7"::Nil, User(poll.admin,""))
    CommandImpl.addAnswerChoice(0, 0::1::Nil,User(poll.admin,"Царь"))
    CommandImpl.addAnswerChoice(0, 0::2::Nil,User(poll.admin,"Царь"))
    CommandImpl.addQuestion("Тестовый вопрос2?", "open", "open"::Nil, User(poll.admin,""))
    CommandImpl.addAnswerOpen(1, "Можно",User(poll.admin,"Царь"))
    CommandImpl.addAnswerOpen(1, "Нельзя",User(poll.admin,"Царь"))
    val expected = "\uD83D\uDC40 Poll *'test'* has 3 questions:\n*0)* Тестовый вопрос? _(multi)_:\n1\n2\n3\n*1)* Тестовый вопрос? _(multi)_:\n5\n6\n7\n*2)* Тестовый вопрос2? _(open)_"
    PollCommand.getView(poll) shouldBe expected
  }



  override protected def afterEach(): Unit = {
    CommandImpl.polls = CommandImpl.polls.empty
    CommandImpl.context = CommandImpl.context.empty
  }
}
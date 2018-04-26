package Bot

import org.scalatest._

class CommandImplTest extends FlatSpec with Matchers with BeforeAndAfterEach {

    "CreatePool" should "return id" in {
      val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:03:26"),Option("13:22:00 18:03:26"))
      r1 shouldBe 0
    }

  "CreatePool" should "return  unic id" in {
    val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:03:26"),Option("13:22:00 18:03:26"))
    val r2 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:03:26"),Option("13:22:00 18:03:26"))
    r1 should not be r2
  }

  "CreatePool" should "correct Pool" in {
    val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option(null),Option(null))
    CommandImpl.search(r1).id shouldBe 0
    CommandImpl.search(r1).name shouldBe "test"
    PollCommand.active(CommandImpl.search(r1), CommandImpl.formatDate.parse("13:22:00 18:03:27") ) shouldBe false
    CommandImpl.search(r1).anonymity shouldBe true
    CommandImpl.search(r1).start_time shouldBe None
  }

  "CreatePool" should "correct Pool #2" in {
    val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:03:26"),Option("13:22:00 18:03:26"))
    CommandImpl.search(r1).id shouldBe 0
    CommandImpl.search(r1).name shouldBe "test"
    PollCommand.active(CommandImpl.search(r1), CommandImpl.getTimeFromFormat("13:22:00 18:03:26")) shouldBe false
    CommandImpl.search(r1).anonymity shouldBe true
    CommandImpl.search(r1).start_time.get shouldBe CommandImpl.formatDate.parse("13:22:00 18:03:26")
    CommandImpl.search(r1).end_time.get shouldBe CommandImpl.formatDate.parse("13:22:00 18:03:26")
  }


  "StartPoll" should "correct start" in {
    val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:03:26"),Option("13:22:00 18:03:27"))
    CommandImpl.startPoll(r1, CommandImpl.getTimeFromFormat("13:22:01 18:03:26")) shouldBe "Уже запущен"
    PollCommand.active(CommandImpl.search(r1),CommandImpl.getTimeFromFormat("13:22:01 18:03:26")) shouldBe true
  }


  "StartPoll" should "correct start without time" in {
    val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option(null),Option("13:22:00 18:03:26"))
    CommandImpl.startPoll(r1,CommandImpl.getTimeFromFormat("13:22:01 18:03:25")) shouldBe "The poll is started successfully"
    PollCommand.active(CommandImpl.search(r1),CommandImpl.getTimeFromFormat("13:22:02 18:03:25")) shouldBe true
  }

  "StartPoll" should "alredy start" in {
    val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),None,Option("13:22:00 18:03:26"))
    CommandImpl.startPoll(r1, CommandImpl.getTimeFromFormat("13:22:00 18:02:16"))
    CommandImpl.startPoll(r1, CommandImpl.getTimeFromFormat("13:22:01 18:02:16")) shouldBe "Уже запущен"
  }

  "StartPoll" should "correct don't start " in {
    val r1 = CommandImpl.createPoll("test", Option("yes"), Option("test"), Option("13:22:00 18:02:16"), Option("13:22:00 18:03:26"))
    CommandImpl.startPoll(r1, CommandImpl.getTimeFromFormat("13:22:01 18:02:17")) shouldBe "Уже запущен"
    PollCommand.active(CommandImpl.search(r1),CommandImpl.getTimeFromFormat("13:22:02 18:03:25")) shouldBe true
  }

  "StartPoll" should "Success start without start time" in {
    val r1 = CommandImpl.createPoll("test", Option("yes"), Option("test"), None, Option("13:22:00 18:03:26"))
    CommandImpl.startPoll(r1, CommandImpl.getTimeFromFormat("13:22:01 18:02:17")) shouldBe "The poll is started successfully"
    PollCommand.active(CommandImpl.search(r1),CommandImpl.getTimeFromFormat("13:22:02 18:03:25")) shouldBe true
  }

  "StartPoll" should  "Error : poll is not exist" in {
    val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:99"),Option("13:22:00 18:03:26"))
    CommandImpl.startPoll(99,CommandImpl.getTimeFromFormat("13:22:02 18:03:25")) shouldBe "Error : poll is not exist"
  }

  "StopPoll" should  "Succes stop with time" in {
    val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:18"),Option("13:22:00 18:03:26"))
    CommandImpl.stopPoll(r1, CommandImpl.getTimeFromFormat("13:22:02 18:03:25")) shouldBe "Error: опрос остановится автоматически"
    PollCommand.active(CommandImpl.search(r1),CommandImpl.getTimeFromFormat("13:22:02 18:03:27")) shouldBe false


  }


  "StopPoll" should  "Succes stop witout time" in {
    val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:18"),Option(null))
    CommandImpl.stopPoll(r1, CommandImpl.getTimeFromFormat("13:22:02 18:03:25")) shouldBe "The poll is stopped successfully"
    PollCommand.active(CommandImpl.search(r1),CommandImpl.getTimeFromFormat("13:22:02 18:03:27")) shouldBe false
  }

  "StopPoll" should  "Unsucces active witout time" in {
    val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:18"),Option(null))
    PollCommand.active(CommandImpl.search(r1),CommandImpl.getTimeFromFormat("13:22:02 18:03:27")) shouldBe true
  }



  "StopPoll" should  "Error: poll is not exist" in {
    val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:18"),Option("13:22:00 18:03:99"))
    CommandImpl.stopPoll(99,CommandImpl.getTimeFromFormat("13:22:02 18:03:27")) shouldBe "Error: poll is not exist"
  }

  "DeletePoll" should  "Success delete" in {
    val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:18"),Option("13:22:00 18:03:99"))
    CommandImpl.deletePoll(r1) shouldBe "Poll deleted successfully"
    CommandImpl.searchOption(r1).isDefined shouldBe false
  }


  "DeletePoll" should  "don't exist" in {
    CommandImpl.deletePoll(99) shouldBe "Error: poll is not exist"
    CommandImpl.searchOption(99).isDefined shouldBe false
  }
  "ListPolls" should  "show" in {
    val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:18"),Option("13:22:00 18:03:99"))

    CommandImpl.listPolls() shouldBe "Current polls: \n 0 :   test\n"
  }

  override protected def afterEach(): Unit = {
    CommandImpl.cleanRep()
    CommandImpl.removeID()
  }
}

package Bot

import java.util.Date

import org.scalactic.source.Position
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
    CommandImpl.search(r1).active shouldBe false
    CommandImpl.search(r1).anonymity shouldBe true
    CommandImpl.search(r1).start_time shouldBe null
  }

  "CreatePool" should "correct Pool #2" in {
    val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:03:26"),Option("13:22:00 18:03:26"))
    CommandImpl.search(r1).id shouldBe 0
    CommandImpl.search(r1).name shouldBe "test"
    CommandImpl.search(r1).active shouldBe false
    CommandImpl.search(r1).anonymity shouldBe true
    CommandImpl.search(r1).start_time shouldBe CommandImpl.formatDate.parse("13:22:00 18:03:26")
    CommandImpl.search(r1).end_time shouldBe CommandImpl.formatDate.parse("13:22:00 18:03:26")
  }


  "StartPoll" should "correct start" in {
    val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:03:26"),Option("13:22:00 18:03:26"))
    CommandImpl.startPoll(r1) shouldBe "The poll is started successfully"
    CommandImpl.search(r1).active shouldBe true
  }


  "StartPoll" should "correct start without time" in {
    val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option(null),Option("13:22:00 18:03:26"))
    CommandImpl.startPoll(r1) shouldBe "The poll is started successfully"
    CommandImpl.search(r1).active shouldBe true
  }

  "StartPoll" should "alredy start" in {
    val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option(null),Option("13:22:00 18:03:26"))
    CommandImpl.startPoll(r1)
    CommandImpl.startPoll(r1) shouldBe "Уже запущен"

  }

  "StartPoll" should "correct don't start " in {
    val r1 = CommandImpl.createPoll("test", Option("yes"), Option("test"), Option("13:22:00 18:02:99"), Option("13:22:00 18:03:26"))
    CommandImpl.startPoll(r1) shouldBe "Error: еще не время"
    CommandImpl.search(r1).active shouldBe false
  }

  "StartPoll" should  "Error : poll is not exist" in {
    val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:99"),Option("13:22:00 18:03:26"))
    CommandImpl.startPoll(99) shouldBe "Error : poll is not exist"
  }

  "StopPoll" should  "Succes stop with time" in {
    val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:18"),Option("13:22:00 18:03:26"))
    CommandImpl.startPoll(r1)
    CommandImpl.stopPoll(r1) shouldBe "The poll is stopped successfully"
    CommandImpl.search(r1).active shouldBe false

  }


  "StopPoll" should  "Succes stop witout time" in {
    val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:18"),Option(null))
    CommandImpl.startPoll(r1)
    CommandImpl.stopPoll(r1) shouldBe "The poll is stopped successfully"
    CommandImpl.search(r1).active shouldBe false
  }

  "StopPoll" should  "Fail stop with time" in {
    val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:18"),Option("13:22:00 18:03:99"))
    CommandImpl.startPoll(r1)
    CommandImpl.stopPoll(r1) shouldBe "Error: еще не время"
  }


  "StopPoll" should  "Fail stop without start" in {
    val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:18"),Option("13:22:00 18:03:99"))
    CommandImpl.stopPoll(r1) shouldBe "Опрос еще не запущен"
  }

  "StopPoll" should  "Error: poll is not exist" in {
    val r1 = CommandImpl.createPoll("test",Option("yes"),Option("test"),Option("13:22:00 18:02:18"),Option("13:22:00 18:03:99"))
    CommandImpl.stopPoll(99) shouldBe "Error: poll is not exist"
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
    CommandImpl.clean()
    CommandImpl.removeID()
  }
}

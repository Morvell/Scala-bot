package Bot

import Bot.Matcher.StartM
import org.scalactic.Fail
import org.scalatest.{FlatSpec, FunSuite, Matchers}


class CommandParserTest extends FlatSpec with Matchers{
  private val parser = CommandParser

 "/create_poll (my_poll) (no) (afterstop))" should Matcher.CreatePoolM.toString() in {
   val result = parser("/create_poll (my_poll) (no) (afterstop))")
   result.get match { case c: Matcher.CreatePoolM =>
     c.name shouldBe "my_poll"
     c.anon shouldBe Some("no")
   }
   result.get shouldBe a [Matcher.CreatePoolM]
 }


  "/create_poll (one_more_poll) (yes) (continuous) (13:22:00 18:03:26)" should Matcher.CreatePoolM.toString() in {
    val result = parser("/create_poll (one_more_poll) (yes) (continuous) (13:22:00 18:03:26)")
    result.get match { case c: Matcher.CreatePoolM =>
      c.name shouldBe "one_more_poll"
      c.anon shouldBe Some("yes")
    }
    result.get shouldBe a [Matcher.CreatePoolM]
  }

  "/create_poll (one_more_poll) (yes) (continuous) (13:22:00 18:03:26) (15:22:00 18:03:26)" should Matcher.CreatePoolM.toString() in {
    val result = parser("/create_poll (one_more_poll) (yes) (continuous) (13:22:00 18:03:26) (15:22:00 18:03:26)")
    result.get shouldBe a [Matcher.CreatePoolM]
  }

  "/start_poll (0)" should Matcher.StartM.toString() in {
    val result = parser("/start_poll (0)")
    result.get match { case c: Matcher.StartM => c.d shouldBe 0}
    result.get shouldBe a [Matcher.StartM]
  }

  "/result (0)" should Matcher.ResultM.toString() in {
    val result = parser("/result (0)")
    result.get match { case c: Matcher.ResultM => c.d shouldBe 0}
    result.get shouldBe a [Matcher.ResultM]
  }

"/list" should Matcher.ListM.toString() in {
    val result = parser("/list")
    result.get shouldBe a [Matcher.ListM]
  }

  "/delete_poll (0)" should Matcher.DeleteM.toString() in {
    val result = parser("/delete_poll (0)")
    result.get match { case c: Matcher.DeleteM => c.d shouldBe 0}
    result.get shouldBe a [Matcher.DeleteM]
  }

  "/stop_poll (1)" should Matcher.StopM.toString() in {
    val result = parser("/stop_poll (1)")
    result.get match { case c: Matcher.StopM => c.d shouldBe 1}
    result.get shouldBe a [Matcher.StopM]
  }

  "/bad_request" should "Fail" in {
    val result = parser("/bad_request")
  }
}

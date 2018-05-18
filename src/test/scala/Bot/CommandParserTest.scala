package Bot

import org.scalatest.{FlatSpec, Matchers}


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

  "/begin (" should "fail" in {
    val result = parser("/begin (")
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
    result.successful shouldBe false
  }



  //////////////////////////////////////////////////////////

  "/begin (1)" should Matcher.BeginM.toString() in {
    val result = parser("/begin (1)")
    result.get match { case c: Matcher.BeginM => c.d shouldBe 1}
    result.get shouldBe a [Matcher.BeginM]
  }


  "/end " should Matcher.EndM.toString() in {
    val result = parser("/end")
    result.get shouldBe a [Matcher.EndM]
  }


  "/view " should Matcher.ViewM.toString() in {
    val result = parser("/view ")
    result.get shouldBe a [Matcher.ViewM]
  }


  "/delete_question " should Matcher.DeleteQuestionM.toString() in {
    val result = parser("/delete_question (1)")
    result.get match { case c: Matcher.DeleteQuestionM => c.d shouldBe 1}
    result.get shouldBe a [Matcher.DeleteQuestionM]
  }

//  "/add_question choice" should Matcher.AddQuestionChoiceM.toString() in {
//    val result = parser("""/add_question (Test case) (choice)
//        |One And A
//        |Two""".stripMargin)
//    result.get match { case c: Matcher.AddQuestionChoiceM =>
//      c.q shouldBe "Test case"
//      c.t shouldBe "choice"
//      c.variant.tail.head shouldBe "Two"
//    }
//    result.get shouldBe a [Matcher.AddQuestionChoiceM]
//  }

//  "/add_question multi" should Matcher.AddQuestionChoiceM.toString() in {
//    val result = parser("""/add_question (Test multi) (multi)
//                          |One And A
//                          |Two""".stripMargin)
//    result.get match { case c: Matcher.AddQuestionChoiceM =>
//      c.q shouldBe "Test multi"
//      c.t shouldBe "multi"
//      c.variant.tail.head shouldBe "Two"
//    }
//    result.get shouldBe a [Matcher.AddQuestionChoiceM]
//  }

  "/add_question open" should Matcher.AddQuestionOpenM.toString() in {
    val result = parser("""/add_question (Test open) (open)""".stripMargin)
    result.get match { case c: Matcher.AddQuestionOpenM =>
      c.q shouldBe "Test open"
      c.t shouldBe "open"
    }
    result.get shouldBe a [Matcher.AddQuestionOpenM]
  }


  //TODO many tests


  "/answer string" should Matcher.AnswerStringM.toString() in {
    val result = parser("""/answer (1) (multi)""")
    result.get match { case c: Matcher.AnswerStringM =>
      c.d shouldBe 1
      c.a shouldBe "multi"
    }
    result.get shouldBe a [Matcher.AnswerStringM]
  }

}

package Bot

import org.scalactic.Fail
import org.scalatest.{FlatSpec, FunSuite}


class CommandParserTest extends FlatSpec {
  private val parser = CommandParser

 "/create_poll (my_poll) (no) (afterstop))" should Matcher.CreatePoolM.toString() in {
   val result = parser("/create_poll (my_poll) (no) (afterstop))").map {
     case c: Matcher.CreatePoolM => Matcher.CreatePoolM
     case _  => Fail
   }
   assertResult(Matcher.CreatePoolM)(result.get)
 }


  "/create_poll (one_more_poll) (yes) (continuous) (13:22:00 18:03:26)" should Matcher.CreatePoolM.toString() in {
    val result = parser("/create_poll (one_more_poll) (yes) (continuous) (13:22:00 18:03:26)").map {
      case c: Matcher.CreatePoolM => Matcher.CreatePoolM
      case _  => Fail
    }
    assertResult(Matcher.CreatePoolM)(result.get)
  }

  "/create_poll (one_more_poll) (yes) (continuous) (13:22:00 18:03:26) (15:22:00 18:03:26)" should Matcher.CreatePoolM.toString() in {
    val result = parser("/create_poll (one_more_poll) (yes) (continuous) (13:22:00 18:03:26) (15:22:00 18:03:26)").map {
      case c: Matcher.CreatePoolM => Matcher.CreatePoolM
      case _  => Fail
    }
    assertResult(Matcher.CreatePoolM)(result.get)
  }

  "/start_poll (0)" should Matcher.StartM.toString() in {
    val result = parser("/start_poll (0)").map {
      case c: Matcher.StartM => Matcher.StartM
      case _  => Fail
    }
    assertResult(Matcher.StartM)(result.get)
  }

  "/result (0)" should Matcher.ResultM.toString() in {
    val result = parser("/result (0)").map {
      case c: Matcher.ResultM => Matcher.ResultM
      case _  => Fail
    }
    assertResult(Matcher.ResultM)(result.get)
  }

"/list" should Matcher.ListM.toString() in {
    val result = parser("/list").map {
      case c: Matcher.ListM => Matcher.ListM
      case _  => Fail
    }
    assertResult(Matcher.ListM)(result.get)
  }

  "/delete_poll (0)" should Matcher.DeleteM.toString() in {
    val result = parser("/delete_poll (0)").map {
      case c: Matcher.DeleteM => Matcher.DeleteM
      case _  => Fail
    }
    assertResult(Matcher.DeleteM)(result.get)
  }

  "/stop_poll (1)" should Matcher.StopM.toString() in {
    val result = parser("/stop_poll (1)").map {
      case c: Matcher.StopM => Matcher.StopM
      case _  => Fail
    }
    assertResult(Matcher.StopM)(result.get)
  }

  //TODO
//  "/bad_request" should "Fail" in {
//    val result = parser("/bad_request")
//    assertResult()(result.get)
//  }





}

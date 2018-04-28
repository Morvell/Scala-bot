import Bot.Poll

import scala.collection.{immutable, mutable}


var polls: immutable.Map[Int, Int] = immutable.Map[Int, Int](1->1)


val a = polls(1)

polls = polls + (1 -> (a + 1))




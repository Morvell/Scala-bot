import Bot.Poll

import scala.collection.{immutable, mutable}


case class Context(a:immutable.Map[Int, Int])
val polls: immutable.Map[Int, Int] = immutable.Map[Int, Int](1->1)


polls.get(1)

val a = Context(polls)

val b = a.copy(a = polls + (2 -> 1))

val c = b.copy(a = b.a + (3 -> 1))




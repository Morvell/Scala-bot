import Bot.Poll

import scala.collection.immutable

var polls = immutable.Map[Int,Int]()

polls = polls + (1 -> 1)
polls = polls + (2 -> 1)

var a = polls.get(1)
print(a.isDefined)
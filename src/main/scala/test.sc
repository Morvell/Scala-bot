import Bot.Poll

import scala.collection.immutable

var polls = immutable.Map[Int,Int]()

polls += 1 -> 1
polls += 1 -> 2

polls.get(1)


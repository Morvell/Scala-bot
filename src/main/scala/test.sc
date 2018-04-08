import Bot.Poll

import scala.collection.immutable

var polls = immutable.Map[Int,Int]()

polls = polls + (1 -> 1)
polls = polls + (2 -> 2)

polls.foreach( b => print(b))

polls.get(1)


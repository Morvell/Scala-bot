package Bot

import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.api.declarative._
import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._


object Telegram extends TelegramBot with Polling with Commands {
  def token = "531765707:AAFIOaj6SKOnBGB7sJiPOmgQ500wldHBsos"

  override def receiveMessage(msg: Message): Unit = {
    val name = s"${msg.from.get.firstName} ${msg.from.get.lastName.getOrElse("")}".trim
    val user = Bot.User(msg.source, name)
    for (text <- msg.text) {
      println(s"Text received: '$text', from: '$name', id: ${msg.from.get.id}")
      request(SendMessage(msg.source, App.responseToLine(text, user), Some(ParseMode.Markdown)))
    }
  }

  def main(a: Array[String]): Unit = {
    println("Started telegram bot")
    run()
  }
}

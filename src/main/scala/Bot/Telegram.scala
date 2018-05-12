import Bot.App
import info.mukel.telegrambot4s._
import api._
import methods._
import models._
import declarative._


object Telegram extends TelegramBot with Polling with Commands {
  def token = "498627475:AAEymtUVj1-KzqOaf17PW2Cf2PacQtRyoig"

  override def receiveMessage(msg: Message): Unit = {
    for (text <- msg.text) {
      println(msg.source)
      request(SendMessage(msg.source, App.responseToLine(text)))
    }
  }

  def main(a: Array[String]) = {
    println("started")
    run()
  }
}

package com.markovchat.chat

import akka.actor.{Props, ActorSystem, Actor}

import java.net.URL

import org.jibble.jmegahal._
import org.htmlcleaner.HtmlCleaner
import org.apache.commons.lang3.StringEscapeUtils

import scala.util.Random
import scalaj.http.Http

case class Result(url: String, title: String)
case class ResponseData(results: List[Result])
case class GoogleResult(responseData: ResponseData)

case object StartLearning
case object LearnMyName
case class AskYouTube(q: List[String])
case class AskWikipedia(q: List[String])
case class AskChaCha(q: List[String])
case class AskBotLibre(q: List[String])
case class AskMegaHal(q: List[String])
case class AskGoogle(q: List[String])

object BotSystem {
  val system = ActorSystem("SlackBotSystem")
  val hal = new JMegaHal()
  val learner =  system.actorOf(Props(classOf[BotLearner]), "xc-bot-learner")
  val random = new Random()
}

class BotLearner extends Actor with BotHandlers {
  var articlesLearned: Long = 0
  var sentencesLearned: Long = 0
  var isLearning: Boolean = true

  private def handleResponse(f: String => Option[String],
                             input: String) = {
    f(input.toLowerCase) match {
      case Some(s) => sender ! s
      case None => sender ! ""
    }
  }

  def receive = {
    case (LearnMyName) => BotSystem.hal.add("My name is xc-bot")
    case (StartLearning) => {
      try {
        if (isLearning) {
          val cleaner = new HtmlCleaner
          val rootNode = cleaner.clean(new URL(BotConfig.urls.randomWiki))
          val elements = rootNode.getElementsByName("p", true)

          for (elem <- elements) {
            val text = StringEscapeUtils.unescapeHtml4(elem.getText.toString)
            text.toString.split("\\.|\\?|\\!").map(t => {
              if (t.trim().length > 15 && !t.trim().contains("[0-9]")) {
                BotSystem.hal.add(t.trim() + ".")
                sentencesLearned += 1
              }
            })
          }
          articlesLearned += 1
        }
      }
      catch {
        case _: Throwable => {
          print("\n\nCan't learn anymore, articles: %s, sentences: %s\n\n"
            .format(articlesLearned, sentencesLearned))
          isLearning = false
        }
      }
    }
    case (ques: AskGoogle) =>
      handleResponse(askGoogle, ques.q.mkString("+"))
    case (ques: AskWikipedia) =>
      handleResponse(askWikipedia, ques.q.mkString("_"))
    case (ques: AskYouTube) =>
      handleResponse(askYouTube, ques.q.mkString("+"))
    case (ques: AskChaCha) =>
      handleResponse(askChaCha, ques.q.mkString("+"))
    case (ques: AskBotLibre) =>
      handleResponse(askBotLibre, ques.q.mkString(" "))
    case (ques: AskMegaHal) => {
      val response = BotSystem.hal.getSentence(ques.q.mkString(" "))
      sender ! response.toString
        .replaceAll(" He ", " I ")
        .replaceAll(" he ", " I ")
        .replaceAll(" She ", " I ")
        .replaceAll(" she ", " I ")
        .replaceAll(" his ", " my ")
        .replaceAll(" her ", " my ")
    }
  }

}

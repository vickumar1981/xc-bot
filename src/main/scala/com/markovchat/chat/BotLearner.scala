package com.markovchat.chat

import akka.actor.{Props, ActorSystem, Actor}

import java.net.URL

import org.jibble.jmegahal._
import org.htmlcleaner.HtmlCleaner
import org.apache.commons.lang3.StringEscapeUtils

import scala.util.Random
import scalaj.http.Http

abstract class BotQuery{ def q: List[String] }
case object LearnFromArticles
case object LearnFromQuotes
case object LearnMyName
case class AskYouTube(q: List[String]) extends BotQuery
case class AskWikipedia(q: List[String]) extends BotQuery
case class AskChaCha(q: List[String]) extends BotQuery
case class AskBotLibre(q: List[String]) extends BotQuery
case class AskMegaHal(q: List[String]) extends BotQuery
case class AskGoogle(q: List[String]) extends BotQuery
case class TellAJoke(q: List[String]=List.empty) extends BotQuery

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
                             input: String="") = {
    f(input.toLowerCase) match {
      case Some(s) => sender ! s
      case None => sender ! ""
    }
  }

  private def handleLearning(f:() => Boolean) = {
    try {
      if (isLearning) {
        f()
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

  private def learnSentence(s: String) = {
    BotSystem.hal.add(s.trim() + ".")
    sentencesLearned += 1
  }

  private def learnFromQuotes() =
    handleLearning(() => {
      val rootNode = cleaner.clean(new URL(BotConfig.urls.randomQuote))
      val elements = rootNode.getElementsByName("li", true)

      for (elem <- elements) {
        val text = StringEscapeUtils.unescapeHtml4(elem.getText.toString)
        if (text.length() > 150 && !text.toLowerCase.contains("terms of use"))
          text.toString.split("\\.|\\?|\\!").map(t =>
            if (!t.contains("published") && !t.contains("quoted"))
              learnSentence(t)
          )
      }
      true
    })

  private def learnFromArticles() =
    handleLearning(() => {
      val rootNode = cleaner.clean(new URL(BotConfig.urls.randomWiki))
      val elements = rootNode.getElementsByName("p", true)

      for (elem <- elements) {
        val text = StringEscapeUtils.unescapeHtml4(elem.getText.toString)
        text.toString.split("\\.|\\?|\\!").map(t => {
          if (t.trim().length > 15 && !t.trim().contains("[0-9]"))
            learnSentence(t)
        })
      }
      true
    })

  def receive = {
    case (LearnMyName) => BotSystem.hal.add("My name is xc-bot")
    case (LearnFromArticles) => learnFromArticles()
    case (LearnFromQuotes) => learnFromQuotes()
    case (ques: TellAJoke) =>
      handleResponse(tellAJoke)
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

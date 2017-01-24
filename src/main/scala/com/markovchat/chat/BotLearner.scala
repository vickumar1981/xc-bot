package com.markovchat.chat

import akka.actor.{Props, ActorSystem, Actor}

import java.net.URL

import org.jibble.jmegahal._
import org.htmlcleaner.HtmlCleaner
import org.apache.commons.lang3.StringEscapeUtils

import scala.collection.mutable.Queue

import scala.util.Random
import scalaj.http.Http

abstract class BotQuery{ def q: List[String] }
case object LearnFromArticles
case object LearnFromQuotes
case object LearnInsults
case object LearnMyName

case class AskYouTube(q: List[String]) extends BotQuery
case class AskWikipedia(q: List[String]) extends BotQuery
case class AskChaCha(q: List[String]) extends BotQuery
case class AskBotLibre(q: List[String]) extends BotQuery
case class AskMegaHal(q: List[String]) extends BotQuery
case class AskGoogle(q: List[String]) extends BotQuery
case class MakeGiphy(q: List[String]) extends BotQuery
case class TellAJoke(q: List[String]=List.empty) extends BotQuery
case class TellAnInsult(q: List[String]=List.empty) extends BotQuery

object BotSystem {
  val system = ActorSystem("SlackBotSystem")
  val hal = new JMegaHal()
  val learner =  system.actorOf(Props(classOf[BotLearner]), "xc-bot-learner")
  val random = new Random()
}

class BotLearner extends Actor with BotHandlers {

  class FiniteQueue[A](q: Queue[A]) {
    def enqueueFinite(elem: A, maxSize: Int = 150): Queue[A] = {
      q += elem
      while (q.size > maxSize) { q.dequeue }
      q
    }
  }

  implicit def queue2finitequeue[A](q: Queue[A]) = new FiniteQueue[A](q)

  var articlesLearned: Long = 0
  var sentencesLearned: Long = 0
  var isLearning: Boolean = true
  var insultsLearned: Queue[String] = Queue[String]()

  private def handleResponse(f: String => Option[String],
                             input: String="") = {
    f(input.toLowerCase) match {
      case Some(s) => {
        println("Sending response: %s".format(s))
        sender ! s.replaceAll("&\\S+;", "")
      }
      case None => {
        println("No response to send.")
        sender ! ""
      }
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

  private def learnInsult() = {
    val resp = Http(BotConfig.urls.insult)
      .header("Content-type", "application/xml").asString.body

    val r = resp.split("<insult>")(1).split("</insult>").head
    if (r.nonEmpty && !r.contains(" mom"))
      insultsLearned.enqueueFinite(r.trim)
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
        val nums = ('0' to '9').toSet
        text.toString.split("\\.|\\?|\\!").map(t => {
          if (t.trim().length > 15 && !t.trim().exists(nums.contains(_)))
            learnSentence(t)
        })
      }
      true
    })

  def receive = {
    case (LearnMyName) => BotSystem.hal.add("My name is xc-bot")
    case (LearnFromArticles) => learnFromArticles()
    case (LearnFromQuotes) => learnFromQuotes()
    case (LearnInsults) => learnInsult()
    case (ques: TellAnInsult) => handleResponse((q: String) => {
      println("Received request to tell insult")
      if (insultsLearned.size > 0) {
        val resp = insultsLearned.dequeue
        Some(resp)
      }
      else None
    })
    case (ques: TellAJoke) => {
      println("Received request to tell a joke")
      handleResponse(tellAJoke)
    }
    case (ques: AskGoogle) => {
      println("Received request to ask google")
      handleResponse(askGoogle, ques.q.mkString("+"))
    }
    case (ques: AskWikipedia) => {
      println("Received request to ask wikipedia")
      handleResponse(askWikipedia, ques.q.mkString("_"))
    }
    case (ques: AskYouTube) => {
      println("Received request to ask youtube")
      handleResponse(askYouTube, ques.q.mkString("+"))
    }
    case (ques: AskChaCha) => {
      println("Received request to ask chacha")
      handleResponse(askChaCha, ques.q.mkString("+"))
    }
    case (ques: AskBotLibre) => {
      println("Received request to ask bot libre")
      handleResponse(askBotLibre, ques.q.mkString(" "))
    }
    case (ques: MakeGiphy) => {
      println("Received request to search giphy")
      handleResponse(makeGiphyImage, ques.q.mkString("-"))
    }
    case (ques: AskMegaHal) => {
      println("Received request to ask megahal")
      val randomIndex = BotSystem.random.nextInt(ques.q.length)
      val response = BotSystem.hal.getSentence(ques.q(randomIndex))
        .replaceAll("\n", " ").replaceAll("\t", " ")
      println("Sending response: %s".format(response.toString))
      sender ! response.toString
    }
  }

}

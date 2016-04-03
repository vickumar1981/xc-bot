package com.markovchat.chat

import java.net.URL

import org.htmlcleaner.HtmlCleaner
import org.apache.commons.lang3.StringEscapeUtils
import org.htmlcleaner.TagNode

import scalaj.http.Http

import net.liftweb.json._
import scala.collection.JavaConversions._

case class Result(url: String, title: String)
case class ResponseData(results: List[Result])
case class GoogleResult(responseData: ResponseData)

case class Joke(id: Long, joke: String)
case class JokeResult(`type`: String, value: Joke, categories: List[String])

trait BotHandlers {
  protected lazy val cleaner = new HtmlCleaner

  private def makeGiphyUrl(q: String): String = "%s/%s".format(BotConfig.urls.giphy, q)

  private def askWikipediaUrl(q: String): String =
    "%s/%s".format(BotConfig.urls.wikiArticle, q)

  private def askChaChaUrl(q: String): String = "%s/%s".format(BotConfig.urls.askChaCha, q)

  private def askYouTubeUrl(q: String): String =
    "%s/results?search_query=%s".format(BotConfig.urls.youTube, q)

  implicit val formats = DefaultFormats

  private def parseHTMLTags(url: String, tag: String, attr: String, startPattern: String) = {
    try {
      val rootNode = cleaner.clean(new URL(url))
      val possibleLinks = scala.collection.mutable.ListBuffer[String]()
      rootNode.evaluateXPath("//%s".format(tag)) match {
        case tagList: Array[Object] => {
          for (t <- tagList) {
            val e = t.asInstanceOf[TagNode]
            val l = e.getAttributeByName(attr).toString
            if (!l.isEmpty && l.startsWith(startPattern))
              possibleLinks += l
          }
          if (!possibleLinks.isEmpty) {
            val index = BotSystem.random.nextInt(possibleLinks.length)
            Some(possibleLinks(index))
          }
          else None
        }
        case _ => None
      }
    }
    catch {
      case _: Throwable => None
    }
  }

  protected def makeGiphyImage(q: String=""): Option[String] =
    parseHTMLTags(makeGiphyUrl(q), "img", "src", "https://media")

  protected def tellAJoke(q: String=""): Option[String] = {
    var tellJokes = true
    var joke = ""
    while (tellJokes) {
      try {
        val resp = Http(BotConfig.urls.joke).asString
        val results = parse(resp.body).extract[JokeResult]

        val isExplicit = results.categories.map(_.toLowerCase).contains("explicit")
        val hasBadWords = BotConfig.badWords.exists(bw => results.value.joke.toLowerCase.contains(bw))
        tellJokes = (BotConfig.censored && (hasBadWords || isExplicit)) ||
          results.value.joke.equalsIgnoreCase("Maybe. Any other questions?")

        if (!tellJokes)
          joke = results.value.joke
      }
      catch {
        case _: Throwable => None
      }
    }
    Some(joke)
  }

  protected def askGoogle(q: String): Option[String] = {
    try {
      val resp = Http(BotConfig.urls.google).param("v", "1.0").param("q", q).asString
      val results = parse(resp.body).extract[GoogleResult]
      Some(results.responseData.results(0).url)
    }
    catch {
      case _: Throwable => None
    }
  }

  protected def askWikipedia(q: String): Option[String] = {
    try {
      val link = askWikipediaUrl(q)
      val rootNode = cleaner.clean(new URL(link))
      rootNode.evaluateXPath("//a") match {
        case tagList: Array[Object] => Some(link)
        case _ => None
      }
    }
    catch {
      case _: Throwable => None
    }
  }

  protected def askYouTube(q: String): Option[String] =
    parseHTMLTags(askYouTubeUrl(q), "a", "href", "/watch")

  protected def askChaCha(q: String): Option[String] = {
    try {
      val rootNode = cleaner.clean(new URL(askChaChaUrl(q)))
      rootNode.evaluateXPath("//p[@class='qaAnswerText']") match {
        case tagList: Array[Object] => {
          val r = StringEscapeUtils.unescapeHtml4(tagList(0).asInstanceOf[TagNode].getText().toString).trim()
          if (r.contains("ChaCha"))
            Some(r.substring(0, r.indexOf("ChaCha")))
          else
            Some(r)
        }
        case _ => None
      }
    }
    catch {
      case _: Throwable => None
    }
  }

  protected def askBotLibre(q: String): Option[String] = {
    try {
      if (!BotConfig.applicationId.isEmpty &&
        !BotConfig.chatInstance.isEmpty) {

        val reqMsg =
          """
            |<chat instance="%s">
            |<application>%s</application>
            |<message>%s</message>
            |</chat>
          """.format(BotConfig.chatInstance,
            BotConfig.applicationId, q).stripMargin

        val resp = Http(BotConfig.urls.botLibre)
          .postData(reqMsg)
          .header("Content-type", "application/xml").asString.body

        val r = resp.split("<message>")(1).split("</message>")(0)
        if (r.isEmpty || r.contains("no idea") ||
          r.contains("do not understand") || r.contains("don't understand"))
          None
        else Some(r)
      }
      else None
    }
    catch {
      case _: Throwable => None
    }
  }

}

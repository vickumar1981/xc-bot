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

  private def askChaChaUrl(q: String): String = "%s?q=%s".format(BotConfig.urls.askChaCha, q)

  private def askYouTubeUrl(q: String): String =
    "%s/results?search_query=%s".format(BotConfig.urls.youTube, q)

  implicit val formats = DefaultFormats

  private def parseHTMLTags(url: String, tag: String, attr: String,
                            startPattern: String, urlPrefix: String = "") = {
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
            val retVal = possibleLinks(index)
            if (urlPrefix.isEmpty) Some(retVal.replaceAll("200_s.gif", "giphy.gif"))
            else Some(urlPrefix + retVal)
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
        case t: Throwable => {
          println("error telling joke: %s".format(t.toString))
          None
        }
      }
    }
    Some(joke)
  }

  protected def askGoogle(q: String): Option[String] = {
    try {
      val resp = Http(BotConfig.urls.google).param("v", "1.0").param("q", q).asString
      val results = parse(resp.body).extract[GoogleResult]
      Some(results.responseData.results.head.url)
    }
    catch {
      case t: Throwable => {
        println("error asking google: %s".format(t.toString))
        None
      }
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
      case t: Throwable => {
        println("error asking wikipedia: %s".format(t.toString))
        None
      }
    }
  }

  protected def askYouTube(q: String): Option[String] =
    parseHTMLTags(askYouTubeUrl(q), "a", "href", "/watch", BotConfig.urls.youTube)

  protected def askChaCha(q: String): Option[String] = {
    try {
      val rootNode = cleaner.clean(new URL(askChaChaUrl(q)))
      rootNode.evaluateXPath("//div[@class='sa_headline_block']") match {
        case tagList: Array[Object] => {
          val r = StringEscapeUtils.unescapeHtml4(tagList.head.asInstanceOf[TagNode].getText().toString).trim()
          Some(r.replaceAll("\n", " ").replaceAll("\t", " "))
        }
        case _ => None
      }
    }
    catch {
      case t: Throwable => {
        println("error asking askchacha: %s".format(t.toString))
        None
      }
    }
  }

  protected def askBotLibre(q: String): Option[String] = {
    try {
      if (!BotConfig.applicationId.isEmpty &&
        !BotConfig.chatInstance.isEmpty) {

        val reqMsg =
          """
            |<chat instance="%s" application="%s">
            |<message>%s</message>
            |</chat>
          """.format(BotConfig.chatInstance,
            BotConfig.applicationId, q).stripMargin

        print("\nbot libre request: %s\n".format(reqMsg))

        val resp = Http(BotConfig.urls.botLibre)
          .postData(reqMsg)
          .header("Content-type", "application/xml").asString.body

        println("\nbot libre response: %s\n".format(resp))

        val r = resp.split("<message>")(1).split("</message>").head
        if (r.isEmpty || r.contains("no idea") ||
          r.contains("do not understand") || r.contains("don't understand"))
          None
        else Some(r)
      }
      else None
    }
    catch {
      case t: Throwable => {
        println("error asking bot libre: %s".format(t.toString))
        None
      }
    }
  }

}

package com.markovchat.chat

import java.util.concurrent.TimeUnit

import akka.util.Timeout
import io.scalac.slack.MessageEventBus
import io.scalac.slack.bots.AbstractBot
import io.scalac.slack.common.{BaseMessage, Command, OutboundMessage}

import akka.pattern.ask

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.Duration

class MarkovChatBot(override val bus: MessageEventBus) extends AbstractBot {

  implicit val timeout = Timeout(Duration.create(60, TimeUnit.SECONDS))

  override def help(channel: String): OutboundMessage =
    OutboundMessage(channel,
      s"$name will chat with you. \\n" +
      "Usage: [? | {message} | [google wiki youtube giphy insult joke] {message}")

  val possibleOperations = Map(
    "+" -> ((x: Double, y: Double) => x+y),
    "-" -> ((x: Double, y: Double) => x-y),
    "*" -> ((x: Double, y: Double) => x*y),
    "/" -> ((x: Double, y: Double) => x/y)
  )

  private def cleanParse(input: List[String]) =
    input.map({
      i => i.replaceAll("[^A-Za-z0-9 ]", "")
        .replaceAll("[ ][ ]+", " and ").toLowerCase
    })

  private def handleCommand(response: Future[Any], message: BaseMessage,
                           isJoke: Boolean=false, isInsult: Boolean=false) = {
    response.onSuccess({
      case msg: String =>
        if (!msg.isEmpty) {
          val r = msg.replaceAll("&quot;", "\"")
          if (isJoke) {
            val r2 = r.replaceAll("Chuck Norris", s"""<@${message.user}>""")
            publish(OutboundMessage(message.channel, r2))
          }
          else if (isInsult) {
            val r2 = s"""<@${message.user}>""" + " " + r
            publish(OutboundMessage(message.channel, r2))
          }
          else publish(OutboundMessage(message.channel, r))
        }
    })
  }

  private def guessVal = BotSystem.random.nextInt(719)

  override def act: Receive = {
    case Command("?", operation :: args, message) if args.length >= 1 => {
      val op = possibleOperations.get(operation)

      val response = op.map(f => {
        val result = args.map(_.toDouble).reduceLeft(f(_, _))
        OutboundMessage(message.channel, s"Results is: $result")
      }).getOrElse(OutboundMessage(message.channel, s"No operation $operation"))

      publish(response)
    }

    case Command("?", _, message) => {
      publish(OutboundMessage(message.channel,
        s"Usage: [? | {message} | [google wiki youtube giphy insult joke] {message}"))
    }

    case Command("giphy", text, message) =>
      handleCommand(
        BotSystem.learner ? MakeGiphy(cleanParse(text)),
        message)

    case Command("wiki", text, message) =>
      handleCommand(
        BotSystem.learner ? AskWikipedia(cleanParse(text)),
        message)

    case Command("youtube", text, message) =>
      handleCommand(
        BotSystem.learner ? AskYouTube(cleanParse(text)),
        message)

    case Command("google", text, message) =>
      handleCommand(
        BotSystem.learner ? AskGoogle(cleanParse(text)),
        message
      )

    case Command(_, text, message) => {
      if (text.map(_.toLowerCase).contains("joke")) {
        handleCommand(
          BotSystem.learner ? TellAJoke(),
          message,
          isJoke = true
        )
      }
      else if (text.map(_.toLowerCase).contains("insult")) {
        handleCommand(
          BotSystem.learner ? TellAnInsult(),
          message,
          isInsult = true
        )
      }
      else {
        val cleaned = cleanParse(text)
        val rChaCha = BotSystem.learner ? AskChaCha(cleaned)
        val rBotLibre = BotSystem.learner ? AskBotLibre(cleaned)
        val rMegaHal = BotSystem.learner ? AskMegaHal(cleaned)

        val commands = List("?", "google", "youtube", "wiki")
        if (!text.isEmpty && !commands.contains(text.head)) {
          val combined = for {
            r1 <- rBotLibre
            r2 <- rChaCha
            r3 <- rMegaHal
          } yield (r1, r2, r3)

          combined.onSuccess({
            case (r1: String, r2: String, r3: String) => {
              val msgToSend: String =
                (if (r1.isEmpty)
                  if (r2.isEmpty || guessVal <= BotSystem.random.nextInt(67)) r3
                  else r2
                else r1)
              publish(OutboundMessage(message.channel, msgToSend))
            }
          })
        }
      }
    }
    case bm: BaseMessage => {
      val cleaned = cleanParse(bm.text.trim.split(' ').toList)
      if (bm.user.equalsIgnoreCase("uslackbot")  && guessVal <= BotSystem.random.nextInt(113)) {
        val r = BotSystem.learner ? TellAnInsult()
        r.onSuccess({
          case (r1: String) => {
            val r2 = s"""<@${bm.user}>""" + " " + r1
            publish(OutboundMessage(bm.channel, r2))
          }
        })
      }
      else if (cleaned.size <= 10 && guessVal <= BotSystem.random.nextInt(71)) {
        val rMakeGiphy = BotSystem.learner ? MakeGiphy(cleaned)

        rMakeGiphy.onSuccess({
          case (r1: String) => publish(OutboundMessage(bm.channel, r1))
        })
      }
    }
  }
}

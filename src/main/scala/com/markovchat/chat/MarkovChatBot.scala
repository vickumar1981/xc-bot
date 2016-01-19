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
      "Usage: ? {chat message} {arguments separated by space}")

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

  private def handleCommand(response: Future[Any], message: BaseMessage) = {
    response.onSuccess({
      case msg: String =>
        if (!msg.isEmpty)
          publish(OutboundMessage(message.channel, msg))
    })
  }

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
      publish(OutboundMessage(message.channel, s"Commands youtube, wiki, google, ?"))
    }

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
      val cleaned = cleanParse(text)
      val rChaCha = BotSystem.learner ? AskChaCha(cleaned)
      val rBotLibre = BotSystem.learner ? AskBotLibre(cleaned)
      val rMegaHal = BotSystem.learner ? AskMegaHal(cleaned)
      val guessVal = BotSystem.random.nextInt(317)

      val commands = List("?", "google", "youtube", "wiki")
      if (!text.isEmpty && !commands.contains(text(0))) {
        val combined = for {
          r1 <- rChaCha
          r2 <- rBotLibre
          r3 <- rMegaHal
        } yield (r1, r2, r3)

        combined.onSuccess({
          case (r1: String, r2: String, r3: String) => {
            val msgToSend =
              (if (r1.isEmpty)
                if (r2.isEmpty || guessVal <= BotSystem.random.nextInt(67)) r3
                else r2
              else r1).toString
            publish(OutboundMessage(message.channel, msgToSend))
          }
        })
      }
    }
  }
}
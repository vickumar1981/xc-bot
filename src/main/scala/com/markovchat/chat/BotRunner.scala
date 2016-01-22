package com.markovchat.chat

import java.util.concurrent.TimeUnit

import akka.actor.{ActorRef, ActorContext, Props}
import io.scalac.slack.api.{BotInfo, Start}
import io.scalac.slack.bots.system.{HelpBot, CommandsRecognizerBot}
import io.scalac.slack.common.actors.SlackBotActor
import io.scalac.slack.common.Shutdownable
import io.scalac.slack.{Config => SlackConfig, BotModules, MessageEventBus}
import io.scalac.slack.websockets.WebSocket

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

object BotRunner extends Shutdownable {
  val eventBus = new MessageEventBus
  val botName = BotConfig.botName
  val slackBot = BotSystem.system.actorOf(Props(classOf[SlackBotActor],
    new MyBotsBundle(), eventBus, this, None), botName)
  var botInfo: Option[BotInfo] = None

  def main(args: Array[String]) {

    println("%s started".format(botName))
    println("With api key: " + SlackConfig.apiKey)

    try {
      BotSystem.learner ! LearnMyName
      if (BotConfig.strategies.contains("articles"))
        BotSystem.system.scheduler.schedule(Duration.create(0, TimeUnit.SECONDS),
          Duration.create(500, TimeUnit.SECONDS),
          BotSystem.learner, LearnFromArticles)

      if (BotConfig.strategies.contains("quotes"))
        BotSystem.system.scheduler.schedule(Duration.create(0, TimeUnit.SECONDS),
          Duration.create(45, TimeUnit.SECONDS),
          BotSystem.learner, LearnFromQuotes)

      slackBot ! Start

      BotSystem.system.awaitTermination()
      println("Shutdown successful...")
    } catch {
      case e: Exception =>
        println("An unhandled exception occurred...", e)
        BotSystem.system.shutdown()
        BotSystem.system.awaitTermination()
    }
  }

  sys.addShutdownHook(shutdown())

  override def shutdown(): Unit = {
    slackBot ! WebSocket.Release
    BotSystem.system.shutdown()
    BotSystem.system.awaitTermination()
  }

  class MyBotsBundle() extends BotModules {
    override def registerModules(context: ActorContext, websocketClient: ActorRef) = {
      context.actorOf(Props(classOf[CommandsRecognizerBot], eventBus), "commandProcessor")
      context.actorOf(Props(classOf[HelpBot], eventBus), "helpBot")
      context.actorOf(Props(classOf[MarkovChatBot], eventBus), "MarkovChatBot")
    }
  }
}

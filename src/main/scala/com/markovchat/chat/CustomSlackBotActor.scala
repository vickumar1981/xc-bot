package com.markovchat.chat

import java.util.concurrent.TimeUnit

import akka.actor.{ActorRef, Actor, ActorLogging, Props}
import akka.util.Timeout
import io.scalac.slack._
import io.scalac.slack.api._
import io.scalac.slack.websockets.WebSocket

import scala.concurrent.duration._

class CustomSlackBotActor(modules: BotModules, eventBus: MessageEventBus, websocketClient: ActorRef) extends Actor with ActorLogging {

  import context.{dispatcher, system}

  val api = context.actorOf(Props[ApiActor])
  val richProcessor = context.actorOf(Props(classOf[OutgoingRichMessageProcessor], api, eventBus))

  var errors = 0

  override def receive: Receive = {
    case Start =>
      //test connection
      log.info("trying to connect to Slack server...")
      api ! ApiTest()

    case Stop =>
      BotRunner.shutdown()

    case Ok =>
      log.info("connected successfully...")
      log.info("trying to auth")
      api ! AuthTest(Config.apiKey)

    case ad: AuthData =>
      log.info("authenticated successfully")
      log.info("request for websocket connection...")
      api ! RtmStart(Config.apiKey)

    case RtmData(url) =>
      log.info("fetched WSS URL")
      log.info(url)
      log.info("trying to connect to websockets channel")
      val dropProtocol = url.drop(6)
      val host = dropProtocol.split('/').head
      val resource = dropProtocol.drop(host.length)

      implicit val timeout: Timeout = 5.seconds

      log.info(s"Connecting to host [$host] and resource [$resource]")

      websocketClient ! WebSocket.Connect(host, 443, resource, withSsl = true)

      context.system.scheduler.scheduleOnce(Duration.create(5, TimeUnit.SECONDS), self, RegisterModules)

    case bi @ BotInfo(_, _) =>
      log.info(s"Received bot info $bi")
      BotRunner.botInfo = Some(bi)

    case RegisterModules =>
      modules.registerModules(context, websocketClient)

    case MigrationInProgress =>
      log.warning("MIGRATION IN PROGRESS, next try for 10 seconds")
      system.scheduler.scheduleOnce(10.seconds, self, Start)

    case se: SlackError if errors < 10 =>
      errors += 1
      log.error(s"connection error [$errors], repeat for 10 seconds")
      log.error(s"SlackError occurred [${se.toString}]")
      system.scheduler.scheduleOnce(10.seconds, self, Start)

    case se: SlackError =>
      log.error(s"SlackError occurred [${se.toString}]")
      BotRunner.shutdown()
  }
}


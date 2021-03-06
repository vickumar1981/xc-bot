package com.markovchat.chat

import com.typesafe.config.ConfigFactory

object BotConfig {

  private lazy val config = ConfigFactory.load()

  lazy val botName = config.getString("bot.name")
  lazy val censored = config.getBoolean("bot.censorship")
  lazy val eavesdropping = config.getBoolean("bot.eavesdropping")
  lazy val strategies = config.getStringList("bot.strategies")
  lazy val badWords = config.getStringList("bot.badWords")

  lazy val applicationId = config.getString("bot.applicationId")
  lazy val chatInstance = config.getString("bot.chatInstance")

  object urls {
    lazy val randomWiki = config.getString("bot.urls.wiki.random")
    lazy val randomQuote = config.getString("bot.urls.wiki.quote")
    lazy val wikiArticle = config.getString("bot.urls.wiki.article")
    lazy val askChaCha = config.getString("bot.urls.askChaCha")
    lazy val youTube = config.getString("bot.urls.youTube")
    lazy val botLibre = config.getString("bot.urls.botLibre")
    lazy val google = config.getString("bot.urls.google")
    lazy val joke = config.getString("bot.urls.joke")
    lazy val giphy = config.getString("bot.urls.giphy")
    lazy val insult = config.getString("bot.urls.insult")
  }
}

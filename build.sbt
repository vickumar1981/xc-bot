import com.typesafe.sbt.SbtStartScript

seq(SbtStartScript.startScriptForClassesSettings: _*)

name := "scala-slack-bot-example"

scalaVersion := "2.11.6"

version := "0.1"

resolvers += "scalac repo" at "https://raw.githubusercontent.com/ScalaConsultants/mvn-repo/master/"

libraryDependencies ++= Seq(
  "io.scalac" %% "slack-scala-bot-core" % "0.2.1",
  "org.apache.commons" % "commons-lang3" % "3.4",
  "org.scalaj" %% "scalaj-http" % "2.2.1",
  "net.liftweb" %% "lift-json" % "2.6",
  "com.typesafe" % "config" % "1.2.1")


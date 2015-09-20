name := "smack-scala"
organization := "com.michaelpollmeier"
version := "1.0.0-SNAPSHOT"
scalaVersion := "2.11.7"
val smackVersion = "4.1.3"
val akkaVersion = "2.3.12"
val awsVersion = "0.5.3"

fork in Test := false

libraryDependencies ++= Seq(
  "org.igniterealtime.smack" % "smack-java7" % smackVersion,
  "org.igniterealtime.smack" % "smack-tcp" % smackVersion,
  "org.igniterealtime.smack" % "smack-im" % smackVersion,
  "org.igniterealtime.smack" % "smack-extensions" % smackVersion,
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion % Test,
  "ch.qos.logback" % "logback-classic" % "1.1.3",
  "org.scalatest" %% "scalatest" % "2.2.4" % Test,

  "com.github.seratch" %% "awscala" % awsVersion,

  // to avoid version conflicts in transitive deps:
  "org.scala-lang" % "scala-reflect" % "2.11.7",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.4"
)

val repo = "https://nexus.prod.corp/content"
publishTo <<= version { (v: String) ⇒
  if (v.trim.endsWith("SNAPSHOT")) Some("snapshots" at s"$repo/repositories/snapshots")
  else Some("releases" at s"$repo/repositories/releases")
}

name := "data-server"

version := "0.1"

scalaVersion := "2.12.13"

resolvers += ("MavenRepository" at "https://mvnrepository.com")
//libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % Test
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.7"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.7" % "test"
libraryDependencies += "com.fasterxml.jackson.module" % "jackson-module-scala" % "2.0.2"
//libraryDependencies += "co.theasi" % "plotly_2.11" % "0.2.0" % "test"
//val AkkaVersion = "2.6.8"
//val AkkaHttpVersion = "10.2.4"
//libraryDependencies ++= Seq(
//  "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
//  "com.typesafe.akka" %% "akka-stream" % AkkaVersion,
//  "com.typesafe.akka" %% "akka-http" % AkkaHttpVersion
//)


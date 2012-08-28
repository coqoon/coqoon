name := "Kopitiam"

version := "0.0.17"

organization := "dk.itu.sdg"

scalaVersion := "2.9.2"

crossScalaVersions := Seq("2.10.0-M7")

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" % "akka-actor" % "2.0.1"

libraryDependencies <+= scalaVersion(sv => sv match {
  case "2.10.0-M7" => "org.scalatest" %% "scalatest" % "1.9-2.10.0-M7-B1"
  case _ => "org.scalatest" %% "scalatest" % "1.6.1"
})

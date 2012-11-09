import sbt._
import Process._
import Keys._

name := "soba"

version := "0.1-SNAPSHOT"

organization := "com.octalmind"

scalaVersion := "2.9.1"

publishArtifact in Test := false

publishMavenStyle := true

publishTo <<= (version) { version: String =>
  val nexus = "http://nexus.scala-tools.org/content/repositories/"
  if (version.trim.endsWith("SNAPSHOT")) {
  	Some(Resolver.file("file",  new File( "../maven-repository/snapshots" )))
  }
  else{
 	 Some(Resolver.file("file",  new File( "../maven-repository/releases" )))
  }
}

pomExtra :=
<licenses>
  <license>
    <name>Apache 2</name>
    <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
    <distribution>repo</distribution>
  </license>
</licenses>

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.6.1" % "test",
  "org.slf4j" % "slf4j-api" % "1.6.4",
  "ch.qos.logback" % "logback-classic" % "1.0.0",
  "com.google.guava" % "guava" % "11.0.2"  
  )

scalacOptions ++= Seq( "-deprecation")



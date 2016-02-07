import org.slf4j.LoggerFactory

import sbt._
import sbt.Keys._
import scala.language.reflectiveCalls

scalaVersion := "2.11.7"
organization := "net.scalax.fsn"
name := "fsn-s2s"
version := "0.0.1"

val logger = LoggerFactory.getLogger("sbt init")

val slickVersion = "3.1.1"
libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick" % slickVersion,
  "com.typesafe.slick" %% "slick-hikaricp" % slickVersion exclude("com.zaxxer", "HikariCP-java6"),
  "com.h2database" % "h2" % "1.4.181",
  "org.slf4j" % "slf4j-simple" % "1.7.13"
)

scalacOptions ++= Seq("-feature", "-deprecation")
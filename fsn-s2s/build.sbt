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
  "com.typesafe.slick" %% "slick-hikaricp" % slickVersion exclude("com.zaxxer", "HikariCP-java6")
)
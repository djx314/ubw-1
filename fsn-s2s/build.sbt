import org.slf4j.LoggerFactory

import sbt._
import sbt.Keys._

scalaVersion := "2.11.8"
organization := "net.scalax.fsn"
name := "fsn-s2s"
version := "0.0.1"

val logger = LoggerFactory.getLogger("sbt init")

val slickVersion = "3.1.1"

val shapelessVersion = "2.3.0"

libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick" % slickVersion,
  "com.typesafe.slick" %% "slick-hikaricp" % slickVersion exclude("com.zaxxer", "HikariCP-java6"),
  "com.h2database" % "h2" % "1.4.181",
  "org.slf4j" % "slf4j-simple" % "1.7.13",
  "com.googlecode.usc" % "jdbcdslog" % "1.0.6.2",
  "com.chuusai" %% "shapeless" % shapelessVersion
)

scalacOptions ++= Seq("-feature", "-deprecation")
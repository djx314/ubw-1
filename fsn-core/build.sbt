import org.slf4j.LoggerFactory

import sbt._
import sbt.Keys._
import scala.language.reflectiveCalls

scalaVersion := "2.11.8"
organization := "net.scalax"
name := "fsn-core"
version := "0.0.1"

scalacOptions ++= Seq("-feature", "-deprecation")

libraryDependencies ++= Seq(
  "com.lihaoyi" % "ammonite-repl_2.11.7" % "0.5.3" % "test"
)
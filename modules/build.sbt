import org.slf4j.LoggerFactory

import sbt._
import sbt.Keys._
import scala.language.reflectiveCalls

val slickVersion = "3.1.1"
val scalazVersion = "7.3.0-M3"

libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick" % slickVersion,
  "com.typesafe.slick" %% "slick-hikaricp" % slickVersion exclude("com.zaxxer", "HikariCP-java6"),
  "net.scalax" %% "poi-collection" % "0.1.1",
  //scalaz
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion,
  "org.scalaz" %% "scalaz-iteratee" % scalazVersion,
  "org.scalaz" %% "scalaz-concurrent" % scalazVersion
)

scalacOptions ++= Seq("-feature", "-deprecation")
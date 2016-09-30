import net.scalax.sbt.CustomSettings
import org.slf4j.LoggerFactory

libraryDependencies += "com.lihaoyi" % "ammonite" % "0.7.7" % "test" cross CrossVersion.full

if (scala.util.Properties.isWin)
  initialCommands in (Test, console) += s"""ammonite.repl.Main.run("repl.frontEnd() = ammonite.repl.frontend.FrontEnd.JLineWindows");""" //Node that it doesn't work in ammonite 0.7.7
else
  initialCommands in (Test, console) += s"""ammonite.Main().run();"""

CustomSettings.baseSettings

lazy val logger = {
  LoggerFactory.getLogger("sbt init")
}

lazy val old = (project in file("./old"))
  .settings(CustomSettings.baseSettings: _*)
  .settings(name := "fsn-old")
  .dependsOn(tempBase)
  .aggregate(tempBase)

lazy val tempBase = (project in file("./temp-base"))
  .settings(CustomSettings.baseSettings: _*)
  .settings(name := "fsn-temp-base")
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe.slick" %% "slick" % slickVersion,
      "com.typesafe.slick" %% "slick-hikaricp" % slickVersion exclude("com.zaxxer", "HikariCP-java6")
    ),
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion),
    libraryDependencies ++= Seq(
      //poi
      "org.apache.poi" % "poi" % poiVersion exclude("stax", "stax-api"),
      "org.apache.poi" % "poi-ooxml" % poiVersion exclude("stax", "stax-api"),
      "org.apache.poi" % "poi-ooxml-schemas" % poiVersion exclude("stax", "stax-api"),

      //joda-time
      "joda-time" % "joda-time" % "2.9.4",
      "org.joda" % "joda-convert" % "1.8.1"
    )
  )

val slickVersion = "3.2.0-M1"
val scalazVersion = "7.3.0-M3"
val circeVersion = "0.5.1"
val poiVersion = "3.15-beta2"

lazy val core = (project in file("."))
  .settings(CustomSettings.baseSettings: _*)
  .settings(name := "fsn-core")
  .dependsOn(tempBase)
  .aggregate(tempBase)
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe.slick" %% "slick" % slickVersion,
      "com.typesafe.slick" %% "slick-hikaricp" % slickVersion exclude("com.zaxxer", "HikariCP-java6")
    ),
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core",
      "io.circe" %% "circe-generic",
      "io.circe" %% "circe-parser"
    ).map(_ % circeVersion),
    libraryDependencies ++= Seq(
      //poi
      "org.apache.poi" % "poi" % poiVersion exclude("stax", "stax-api"),
      "org.apache.poi" % "poi-ooxml" % poiVersion exclude("stax", "stax-api"),
      "org.apache.poi" % "poi-ooxml-schemas" % poiVersion exclude("stax", "stax-api"),

      //joda-time
      "joda-time" % "joda-time" % "2.9.4",
      "org.joda" % "joda-convert" % "1.8.1"
    )
  )
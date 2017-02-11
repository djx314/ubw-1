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

/*lazy val old = (project in file("./old"))
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
  )*/

val slickVersion = "3.2.0-RC1"
val circeVersion = "0.7.0"
val poiVersion = "3.15"
val scalazVersion = "7.3.0-M5"

lazy val `fsn-circe` = (project in file("./fsn-circe"))
  .settings(CustomSettings.baseSettings: _*)
  .settings(name := "fsn-circe")
  //.dependsOn(tempBase)
  //.aggregate(tempBase)
  .dependsOn(`fsn-core`)
  .aggregate(`fsn-core`)
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
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.0" % "test",
      "com.h2database" % "h2" % "1.4.192" % "test",
      "org.slf4j" % "slf4j-simple" % "1.7.21" % "test"
    ),
    //scalaz
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % scalazVersion,
      "org.scalaz" %% "scalaz-effect" % scalazVersion,
      "org.scalaz" %% "scalaz-iteratee" % scalazVersion,
      "org.scalaz" %% "scalaz-concurrent" % scalazVersion
    )
  )

lazy val `fsn-core` = (project in file("./fsn-core"))
  .settings(CustomSettings.baseSettings: _*)
  .settings(name := "fsn-core")
  .settings(
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.2",
      scalaOrganization.value % "scala-reflect" % scalaVersion.value
    )
  )
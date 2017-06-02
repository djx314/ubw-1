import net.scalax.sbt.CustomSettings

val slickVersion = "3.2.0"
val circeVersion = "0.7.0"
val poiVersion = "3.15"
val scalazVersion = "7.3.0-M7"

lazy val `fsn-circe` = (project in file("./fsn-circe"))
.settings(CustomSettings.baseSettings: _*)
.settings(name := "fsn-circe")
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
      //"org.typelevel" %% "cats" % "0.9.0",
      scalaOrganization.value % "scala-reflect" % scalaVersion.value
    )
  )
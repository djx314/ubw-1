val slickVersion = "3.2.0"
val circeVersion = "0.7.0"
val scalazVersion = "7.3.0-M14"

name := "ubw-circe"

libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick" % slickVersion,
  "com.typesafe.slick" %% "slick-hikaricp" % slickVersion exclude("com.zaxxer", "HikariCP-java6")
)

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "com.h2database" % "h2" % "1.4.192" % "test",
  "org.slf4j" % "slf4j-simple" % "1.7.21" % "test"
)

//scalaz
libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion,
  "org.scalaz" %% "scalaz-iteratee" % scalazVersion,
  "org.scalaz" %% "scalaz-concurrent" % scalazVersion
)


lazy val `ubw-core` = (project in file("../ubw-core"))
  .settings(net.scalax.sbt.CustomSettings.customSettings: _*)

dependsOn(`ubw-core`)

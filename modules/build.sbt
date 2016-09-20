val slickVersion = "3.2.0-M1"
val scalazVersion = "7.3.0-M3"
val circeVersion = "0.5.1"
val poiVersion = "3.15-beta2"

libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick" % slickVersion,
  "com.typesafe.slick" %% "slick-hikaricp" % slickVersion exclude("com.zaxxer", "HikariCP-java6"),
  //"net.scalax" %% "poi-collection" % "0.1.1",
  //scalaz
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion,
  "org.scalaz" %% "scalaz-iteratee" % scalazVersion,
  "org.scalaz" %% "scalaz-concurrent" % scalazVersion
)

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

libraryDependencies ++= Seq(
  //poi
  "org.apache.poi" % "poi" % poiVersion exclude("stax", "stax-api"),
  "org.apache.poi" % "poi-ooxml" % poiVersion exclude("stax", "stax-api"),
  "org.apache.poi" % "poi-ooxml-schemas" % poiVersion exclude("stax", "stax-api"),

  //joda-time
  "joda-time" % "joda-time" % "2.9.4",
  "org.joda" % "joda-convert" % "1.8.1"
)
/*libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.1"
)

val slickVersion = "3.2.0-M2"
val scalazVersion = "7.3.0-M5"
val circeVersion = "0.6.1"
//val poiVersion = "3.15"
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
).map(_ % circeVersion)*/
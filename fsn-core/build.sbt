libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.1"
)

val circeVersion = "0.5.0-M1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)
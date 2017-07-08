name := "fsn-core"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2",
  scalaOrganization.value % "scala-reflect" % scalaVersion.value
)
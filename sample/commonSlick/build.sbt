resolvers ++= Seq(
  "mavenRepoJX" at "http://repo1.maven.org/maven2/",
  //"bintray/non" at "http://dl.bintray.com/non/maven",
  Resolver.sonatypeRepo("release"),
  Resolver.url("typesafe-ivy", url("http://repo.typesafe.com/typesafe/ivy-releases/"))(Resolver.ivyStylePatterns)
)

externalResolvers := Resolver.withDefaultResolvers(resolvers.value, mavenCentral = false)

lazy val fsnProject = RootProject(file("../../../fsn"))

scalaVersion := "2.12.2"

scalacOptions ++= Seq("-feature", "-deprecation", "-Ywarn-unused-import")

val circeVersion = "0.7.0"

libraryDependencies ++= Seq(
  "com.h2database" % "h2" % "1.4.192",
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.slf4j" % "slf4j-simple" % "1.7.25"
)

val commonSlick = (project in file(".")).dependsOn(fsnProject)
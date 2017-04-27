resolvers ++= Seq(
  "mavenRepoJX" at "http://repo1.maven.org/maven2/",
  //"bintray/non" at "http://dl.bintray.com/non/maven",
  Resolver.sonatypeRepo("release"),
  Resolver.url("typesafe-ivy", url("http://repo.typesafe.com/typesafe/ivy-releases/"))(Resolver.ivyStylePatterns)
)

externalResolvers := Resolver.withDefaultResolvers(resolvers.value, mavenCentral = false)

lazy val fsnProject = RootProject(file("../../../fsn"))

scalaVersion := "2.12.2"

scalacOptions ++= Seq("-feature", "-deprecation")

val commonSlick = (project in file(".")).dependsOn(fsnProject)
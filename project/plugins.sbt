resolvers ++= Seq(
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
  "mavenRepoJX" at "http://repo1.maven.org/maven2/",
  Resolver.typesafeRepo("releases")
)

externalResolvers := Resolver.withDefaultResolvers(resolvers.value, mavenCentral = false)

//addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.9.3")

addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.6.0")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.0")
//addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-M15-1")
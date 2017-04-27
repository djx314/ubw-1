resolvers ++= Seq(
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
  "mavenRepoJX" at "http://repo1.maven.org/maven2/",
  "jgit-repo" at "http://download.eclipse.org/jgit/maven",
  Resolver.typesafeRepo("releases")
)

externalResolvers := Resolver.withDefaultResolvers(resolvers.value, mavenCentral = false)

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.8.5")

addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.6.0")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.4.0")
//addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-M15-1")
//addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.2.0-M5")
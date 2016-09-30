package net.scalax.sbt

import sbt._
import sbt.Keys._

object CustomSettings {
  
  def customSettings = scalaSettings ++ resolversSettings ++ extAlias ++ projectVersion

  def baseSettings = projectVersion ++ scalaSettings

  def projectVersion = Seq(
    organization := "net.scalax",
    version := "0.0.3-M3"
  )

  def scalaSettings =
    Seq(
      scalaVersion := "2.11.8",
      scalacOptions ++= Seq("-feature", "-deprecation")
    )
  
  def resolversSettings =
    Seq(
      resolvers ++= Seq(
        "mavenRepoJX" at "http://repo1.maven.org/maven2/",
        "bintray/non" at "http://dl.bintray.com/non/maven",
        Resolver.sonatypeRepo("release"),
        Resolver.url("typesafe-ivy", url("http://repo.typesafe.com/typesafe/ivy-releases/"))(Resolver.ivyStylePatterns)
      ),
      externalResolvers := Resolver.withDefaultResolvers(resolvers.value, mavenCentral = false)
    )

  def extAlias = addCommandAlias("xeclipse", "eclipse with-source=true skip-parents=false")


}
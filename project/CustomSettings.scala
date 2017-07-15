package net.scalax.sbt

import sbt._
import sbt.Keys._

object CustomSettings {
  
  def customSettings = scalaSettings ++ resolversSettings ++ projectSettings

  def projectSettings = Seq(
    organization := "net.scalax"
  )

  def scalaSettings =
    Seq(
      scalaVersion := "2.11.11",
      scalacOptions ++= Seq("-feature", "-deprecation", "-Ywarn-unused-import", "-language:existentials")
    )
  
  def resolversSettings =
    Seq(
      resolvers ++= Seq(
        "mavenRepoJX" at "http://repo1.maven.org/maven2/",
        //"bintray/non" at "http://dl.bintray.com/non/maven",
        Resolver.sonatypeRepo("release"),
        Resolver.url("typesafe-ivy", url("http://repo.typesafe.com/typesafe/ivy-releases/"))(Resolver.ivyStylePatterns)
      ),
      externalResolvers := Resolver.withDefaultResolvers(resolvers.value, mavenCentral = false)
    )

}
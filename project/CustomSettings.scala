package net.scalax.sbt

import sbt._
import sbt.Keys._

object CustomSettings {
  
  def customSettings = scalaSettings ++ resolversSettings ++ extAlias ++ projectSettings

  def baseSettings = scalaSettings ++ projectSettings

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
  
  def extAlias = List(
    if (scala.util.Properties.isWin)
      addCommandAlias("windowsGitInit",
        """|;
          |git config --global i18n.commitencoding utf-8;
          |git config --global i18n.logoutputencoding gbk;
          |git config --global core.autocrlf true
        """.stripMargin)
    else
      addCommandAlias("linuxGitInit",
        """|;
          |git config --global i18n.commitencoding utf-8;
          |git config --global i18n.logoutputencoding utf-8;
          |git config --global core.autocrlf true;
          |git config core.editor gedit
        """.stripMargin)
  ).flatten

}
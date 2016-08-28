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
        //Resolver.mavenLocal,
        Resolver.sonatypeRepo("release"),
        Resolver.url("typesafe-ivy", url("http://repo.typesafe.com/typesafe/ivy-releases/"))(Resolver.ivyStylePatterns)
      ),
      externalResolvers := Resolver.withDefaultResolvers(resolvers.value, mavenCentral = false)
    )
  
  def extAliasInfo = List(
    Option("xeclipse" -> "eclipse with-source=true skip-parents=false"),
    if (scala.util.Properties.isWin)
      Option(windowsGitInitCommandMap)
    else
      Option(linuxGitInitCommandMap)
  )

  def extAlias = extAliasInfo.collect { case Some(s) => s }
    .foldLeft(List.empty[Def.Setting[_]]){ (s, t) => s ++ addCommandAlias(t._1, t._2) }

  //git init command
  lazy val windowsGitInitCommandMap = "windowsGitInit" ->
    """|;
      |git config --global i18n.commitencoding utf-8;
      |git config --global i18n.logoutputencoding gbk;
      |git config --global core.autocrlf true;
      |git config core.editor \"extras/commit_notepad.bat\"
    """.stripMargin

  lazy val linuxGitInitCommandMap = "linuxGitInit" ->
    """|;
      |git config --global i18n.commitencoding utf-8;
      |git config --global i18n.logoutputencoding utf-8;
      |git config --global core.autocrlf true;
      |git config core.editor gedit
    """.stripMargin

}
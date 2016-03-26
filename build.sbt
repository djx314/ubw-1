import org.slf4j.LoggerFactory

import sbt._
import sbt.Keys._
import scala.language.reflectiveCalls

scalaVersion := "2.11.8"
organization := "net.scalax.fsn"
name := "fsn-parent"
version := "0.0.1"

scalacOptions ++= Seq("-feature", "-deprecation")

libraryDependencies ++= Seq(
  //"com.lihaoyi" % "ammonite-repl" % "0.5.3" % "test" cross CrossVersion.full
  "com.lihaoyi" % "ammonite-repl_2.11.7" % "0.5.3" % "test"
)

extAliasInfo.collect { case Some(s) => s }
.foldLeft(List.empty[Def.Setting[_]]){ (s, t) => s ++ addCommandAlias(t._1, t._2) }

if (OSName.isWindows)
  initialCommands in (Test, console) += s"""ammonite.repl.Main.run("repl.frontEnd() = ammonite.repl.frontend.FrontEnd.JLineWindows");"""
else if (OSName.isLinux)
  initialCommands in (Test, console) += s"""ammonite.repl.Main.run("");"""
else
  initialCommands in (Test, console) += s"""ammonite.repl.Main.run("");"""

lazy val logger = {
  LoggerFactory.getLogger("sbt init")
}

lazy val s2s = (project in file("./fsn-s2s"))
  .dependsOn(core)

lazy val core = (project in file("."))


lazy val OSName = new {
  val OS = System.getProperty("os.name").toLowerCase
  def isLinux = OS.indexOf("linux") >= 0
  def isMacOS = OS.indexOf("mac") >= 0 && OS.indexOf("os") > 0 && OS.indexOf("x") < 0
  def isMacOSX = OS.indexOf("mac") >= 0 && OS.indexOf("os") > 0 && OS.indexOf("x") > 0
  def isWindows = OS.indexOf("windows") >= 0
  def isOS2 = OS.indexOf("os/2") >= 0
  def isSolaris = OS.indexOf("solaris") >= 0
  def isSunOS = OS.indexOf("sunos") >= 0
  def isMPEiX = OS.indexOf("mpe/ix") >= 0
  def isHPUX = OS.indexOf("hp-ux") >= 0
  def isAix = OS.indexOf("aix") >= 0
  def isOS390 = OS.indexOf("os/390") >= 0
  def isFreeBSD = OS.indexOf("freebsd") >= 0
  def isIrix = OS.indexOf("irix") >= 0
  def isDigitalUnix = OS.indexOf("digital") >= 0 && OS.indexOf("unix") > 0
  def isNetWare = OS.indexOf("netware") >= 0
  def isOSF1 = OS.indexOf("osf1") >= 0
  def isOpenVMS = OS.indexOf("openvms") >= 0
}

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

lazy val extAliasInfo = List(
  Option("xeclipse" -> "eclipse with-source=true skip-parents=false"),
  if (OSName.isWindows)
    Option(windowsGitInitCommandMap)
  else if (OSName.isLinux)
    Option(linuxGitInitCommandMap)
  else None
)
import sbt._
import sbt.Keys._
import scala.language.reflectiveCalls

scalaVersion := "2.11.7"
organization := "net.scalax.fsn"
name := "fsn"
version := "0.0.1"

val OSName = new {
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

libraryDependencies += "com.lihaoyi" % "ammonite-repl" % "0.5.3" % "test" cross CrossVersion.full

if (OSName.isWindows)
  initialCommands in (Test, console) += s"""ammonite.repl.Main.run("repl.frontEnd() = ammonite.repl.frontend.FrontEnd.JLineWindows");"""
else if (OSName.isLinux)
  initialCommands in (Test, console) += s"""ammonite.repl.Main.run("");"""
else
  initialCommands in (Test, console) += s""""""

val printlnDo = println("""
fate stay night
""".stripMargin)

//git init command
val windowsGitInitCommandMap = "windowsGitInit" ->
  """|;
    |git config --global i18n.commitencoding utf-8;
    |git config --global i18n.logoutputencoding gbk;
    |git config --global core.autocrlf true;
    |git config core.editor \"extras/commit_note.bat\"
  """.stripMargin

val linuxGitInitCommandMap = "linuxGitInit" ->
  """|;
    |git config --global i18n.commitencoding utf-8;
    |git config --global i18n.logoutputencoding utf-8;
    |git config --global core.autocrlf true;
    |git config core.editor gedit
  """.stripMargin

val extAliasInfo = List(
  Option("xeclipse" -> "eclipse with-source=true skip-parents=false"),
  if (OSName.isWindows)
    Option(windowsGitInitCommandMap)
  else if (OSName.isLinux)
    Option(linuxGitInitCommandMap)
  else None
)

extAliasInfo.collect { case Some(s) => s }
  .foldLeft(List.empty[Def.Setting[_]]){ (s, t) => s ++ addCommandAlias(t._1, t._2) }

val gitInit = taskKey[String]("gitInit")

/*gitInit := {

  val runtime = java.lang.Runtime.getRuntime

  import scala.io.Source
  if (OSName.isWindows) {
    val commandLine = Source.fromFile("./gitUpdate").getLines.map(s => s.replaceAll("\\r\\n", "")).mkString(" & ")
    val process = runtime.exec(List("cmd", "/c", commandLine).toArray)
    execCommonLine(process)
  } else {
    val commandLine = Source.fromFile("./gitUpdate").getLines.map(s => s.replaceAll("\\r\\n", "")).mkString(" ; ")
    val process = runtime.exec(List("sh", "-c", commandLine).toArray)
    execCommonLine(process)
  }
  "git init success."

}*/
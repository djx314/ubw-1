import net.scalax.sbt.CustomSettings
import org.slf4j.LoggerFactory

libraryDependencies += "com.lihaoyi" % "ammonite" % "0.7.7" % "test" cross CrossVersion.full

if (scala.util.Properties.isWin)
  initialCommands in (Test, console) += s"""ammonite.repl.Main.run("repl.frontEnd() = ammonite.repl.frontend.FrontEnd.JLineWindows");""" //Node that it doesn't work in ammonite 0.7.7
else
  initialCommands in (Test, console) += s"""ammonite.Main().run();"""

CustomSettings.baseSettings

lazy val logger = {
  LoggerFactory.getLogger("sbt init")
}

lazy val core = (project in file("./old"))
  .settings(CustomSettings.baseSettings: _*)
  .settings(name := "fsn-core")
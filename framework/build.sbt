import net.scalax.sbt.CustomSettings
import org.slf4j.LoggerFactory

libraryDependencies += "com.lihaoyi" % "ammonite" % "0.7.7" % "test" cross CrossVersion.full

if (scala.util.Properties.isWin)
  initialCommands in (Test, console) += s"""ammonite.repl.Main.run("repl.frontEnd() = ammonite.repl.frontend.FrontEnd.JLineWindows");"""
else
  initialCommands in (Test, console) += s"""ammonite.Main().run();"""

lazy val logger = {
  LoggerFactory.getLogger("sbt init")
}

lazy val modules = (project in file("./old/modules"))
  .settings(CustomSettings.baseSettings: _*)
  .settings(
    name := "fsn-modules",
    addCompilerPlugin(
      "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full
    )
  )
  .dependsOn(core)
  .aggregate(core)

lazy val core = (project in file("./old/fsn-core"))
  .settings(CustomSettings.baseSettings: _*)
  .settings(name := "fsn-core")
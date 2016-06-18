import net.scalax.sbt.CustomSettings
import org.slf4j.LoggerFactory

name := "fsn-parent"

scalacOptions ++= Seq("-feature", "-deprecation")

libraryDependencies ++= Seq(
  "com.lihaoyi" % "ammonite-repl" % "0.6.2" % "test" cross CrossVersion.full
)

if (scala.util.Properties.isWin)
  initialCommands in (Test, console) += s"""ammonite.repl.Main.run("repl.frontEnd() = ammonite.repl.frontend.FrontEnd.JLineWindows");"""
else
  initialCommands in (Test, console) += s"""ammonite.repl.Main.run("");"""

lazy val logger = {
  LoggerFactory.getLogger("sbt init")
}

lazy val modules = (project in file("./modules"))
  .settings(CustomSettings.baseSettings: _*)
    .settings(name := "fsn-modules")
  .dependsOn(core)

lazy val core = (project in file("./fsn-core"))
  .settings(CustomSettings.baseSettings: _*)
  .settings(name := "fsn-core")

lazy val fsn = (project in file(".")).aggregate(modules, core)
.settings(CustomSettings.customSettings: _*)
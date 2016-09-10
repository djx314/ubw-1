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
  .settings(
    name := "fsn-modules",
    addCompilerPlugin(
      "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full
    )
  )
  .dependsOn(core)

lazy val core = (project in file("./fsn-core"))
  .settings(CustomSettings.baseSettings: _*)
  .settings(name := "fsn-core")

lazy val autoGit = taskKey[Unit]("wang")

lazy val fsn = (project in file("."))
  .aggregate(modules, core)
  .settings(CustomSettings.customSettings: _*)
  .settings(
    //libraryDependencies += "net.scalax" %% "jfxgit" % "0.0.2-M1",
    libraryDependencies ++= {
      val jgitVersion = "4.4.1.201607150455-r"
      ("org.scalafx" %% "scalafx" % "8.0.92-R10") ::
      (
        List(
          "org.eclipse.jgit" % "org.eclipse.jgit",
          "org.eclipse.jgit" % "org.eclipse.jgit.pgm",
          "org.eclipse.jgit" % "org.eclipse.jgit.http.server",
          "org.eclipse.jgit" % "org.eclipse.jgit.ui",
          "org.eclipse.jgit" % "org.eclipse.jgit.junit"
        ) map (
          _ % jgitVersion
            exclude("javax.jms", "jms")
            exclude("com.sun.jdmk", "jmxtools")
            exclude("com.sun.jmx", "jmxri")
            exclude("org.slf4j", "slf4j-log4j12")
          )
        )
    },
    autoGit <<= (
      baseDirectory in ThisBuild,
      fullClasspath in Compile,
      javaHome in Compile,
      connectInput in Compile,
      outputStrategy in Compile,
      javaOptions in Compile,
      envVars in Compile
      ) map { (baseDir, fullCp, jHome, cInput, sOutput, jOpts, envs) =>
      val forkOptions: ForkOptions =
        ForkOptions(
          workingDirectory = Some(baseDir),
          bootJars = List(new java.io.File(System.getenv("JAVA_HOME"), "/jre/lib/ext/jfxrt.jar")).filter(_.exists) ++: fullCp.files,
          javaHome = jHome,
          connectInput = cInput,
          outputStrategy = sOutput,
          runJVMOptions = jOpts,
          envVars = envs
        )
      val baseDirPath = baseDir.getAbsolutePath
      println(s"以 $baseDirPath 为根目录运行 jfxgit")
      new Fork("java", Option("org.xarcher.jfxgit.Jfxgit")).apply(forkOptions, Array(baseDirPath))
      ()
    }
  )
import net.scalax.sbt.CustomSettings
import org.slf4j.LoggerFactory

lazy val logger = {
  LoggerFactory.getLogger("sbt init")
}

lazy val fsn = (project in file("."))
  .settings(CustomSettings.customSettings: _*)
  .settings(
    name := "fsn-parent",
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
    }, {
      lazy val autoGit = taskKey[Unit]("To run a git gui.")
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
    },
    addCommandAlias("pack", ";tempBase/clean;old/clean;core/clean;old/package;core/package")
  )

lazy val framework = (project in file("./framework"))
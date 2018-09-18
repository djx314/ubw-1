import net.scalax.sbt.CustomSettings

transitiveClassifiers in ThisBuild := Seq("sources", "javadoc")

CustomSettings.customSettings

name := "ubw-parent"

addCommandAlias("t", "allTest")
addCommandAlias("allTest", "fsn-circe/test")
addCommandAlias("allReport", ";fsn-core/coverageReport;fsn-circe/coverageReport")
addCommandAlias("allClean", ";fsn-core/clean;fsn-circe/clean;framework/clean")


lazy val framework = (project in file("./framework"))
  .settings(CustomSettings.customSettings: _*)

dependsOn(framework)
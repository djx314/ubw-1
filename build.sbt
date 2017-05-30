import net.scalax.sbt.CustomSettings

transitiveClassifiers in ThisBuild := Seq("sources", "javadoc")

lazy val fsn = (project in file("."))
  .settings(CustomSettings.customSettings: _*)
  .settings(
    name := "fsn-parent",
    addCommandAlias("t", "allTest"),
    addCommandAlias("allTest", "fsn-circe/test"),
    addCommandAlias("allReport", ";fsn-core/coverageReport;fsn-circe/coverageReport"),
    addCommandAlias("allClean", ";fsn-core/clean;fsn-circe/clean;framework/clean")
  )
  .dependsOn(LocalProject("fsn-circe"))

lazy val framework = (project in file("./framework"))
  .settings(CustomSettings.customSettings: _*)
  .settings(name := "framework")
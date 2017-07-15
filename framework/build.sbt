val slickVersion = "3.2.0"
val circeVersion = "0.7.0"
val poiVersion = "3.15"
val scalazVersion = "7.3.0-M7"

name := "framework"

lazy val `fsn-circe` = (project in file("./fsn-circe"))
.settings(net.scalax.sbt.CustomSettings.customSettings: _*)
.dependsOn(`poi-collection`)

dependsOn(`fsn-circe`)

lazy val `poi-collection` = RootProject(file("../poi-collection"))
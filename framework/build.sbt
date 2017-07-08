import net.scalax.sbt.CustomSettings

val slickVersion = "3.2.0"
val circeVersion = "0.7.0"
val poiVersion = "3.15"
val scalazVersion = "7.3.0-M7"

lazy val `fsn-circe` = (project in file("./fsn-circe"))
.settings(CustomSettings.baseSettings: _*)
.dependsOn(`poi-collection`)
.dependsOn(`fsn-core`)
.aggregate(`fsn-core`)

lazy val `fsn-core` = (project in file("./fsn-core"))
.settings(CustomSettings.baseSettings: _*)

lazy val `poi-collection` = RootProject(file("../poi-collection"))
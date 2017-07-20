name := "framework"

lazy val `ubw-circe` = (project in file("./ubw-circe"))
.settings(net.scalax.sbt.CustomSettings.customSettings: _*)
.dependsOn(`poi-collection`)

dependsOn(`ubw-circe`)

lazy val `poi-collection` = RootProject(file("../poi-collection"))
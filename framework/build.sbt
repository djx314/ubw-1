name := "framework"

lazy val `ubw-circe` = (project in file("./ubw-circe"))
.settings(net.scalax.sbt.CustomSettings.customSettings: _*)

dependsOn(`ubw-circe`)
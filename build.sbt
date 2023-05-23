lazy val root = project
  .in(file("."))
  .settings(
    scalaVersion := "3.2.2",
    name         := "zkat",
    organization := "com.github.svschen",
    version      := "0.1.0-SNAPSHOT",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-prelude" % "1.0.0-RC19",
      "dev.zio" %% "zio-http" % "3.0.0-RC1"
    )
  )

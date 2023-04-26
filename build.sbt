lazy val root = project
  .in(file("."))
  .settings(
    scalaVersion := "3.2.2",
    name         := "zkat",
    organization := "com.github.svschen",
    version      := "0.1.0-SNAPSHOT",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-prelude" % "1.0.0-RC18"
      /* "io.d11"                       %% "zhttp"        % "2.0.0-RC7", */
    )
  )

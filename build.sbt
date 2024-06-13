val scala3Version = "3.5.0-RC1"
val sttpVersion   = "3.8.3"
val zioVersion    = "1.0.15"
val zioPreludeVersion = "1.0.0-RC8"

lazy val root = project
  .in(file("."))
  .settings(
    name := "math-with-scala",
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "com.lihaoyi"                   %% "scalatags"     % "0.12.0" cross CrossVersion.for3Use2_13,
      "dev.zio"                       %% "zio"           % zioVersion,
      "dev.zio"                       %% "zio-prelude"   % zioPreludeVersion,
      "dev.zio"                       %% "zio-json"      % "0.2.0-M4",
      "com.softwaremill.sttp.client3" %% "core"          % sttpVersion,
      "com.softwaremill.sttp.client3" %% "zio1-json"     % sttpVersion,
      "guru.nidi"                     %  "graphviz-java" % "0.18.1",
      "com.devskiller.friendly-id"    %  "friendly-id"   % "1.1.0"
    ),
  )

scalacOptions ++= Seq(
  "-Xkind-projector:underscores",
  "-source:future",
  "-deprecation",
  "-feature",
)

val scala3Version = "3.1.2"
val catsVersion = "2.6.1"
val scala2Version = "2.13"
val sttpVersion = "3.3.14"
val zioVersion = "1.0.12"

def scala2(s: String) =  s + "_" + scala2Version

lazy val root = project
  .in(file("."))
  .settings(
    name := "math-with-scala",
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "com.novocode"               % "junit-interface"                   % "0.11"  % "test",
      ("org.typelevel"             %% "cats-core"                 % catsVersion),
      ("org.typelevel"             %% "cats-laws"                 % catsVersion),
      "eu.timepit"                 % scala2("refined")                   % "0.9.10",
      "com.lihaoyi"                % scala2("scalatags")                 % "0.8.2",
      "dev.zio"                    %% "zio"                       % zioVersion,
      "com.softwaremill.sttp.client3" %% "core"                   % sttpVersion,
      "com.softwaremill.sttp.client3" %% "circe"                  % sttpVersion,
      "guru.nidi"                  % "graphviz-java"                     % "0.18.1",
      "com.devskiller.friendly-id" % "friendly-id" % "1.1.0"
    ),
  )

scalacOptions ++= Seq(
  "-Ykind-projector:underscores",
//  "-language:strictEquality"
)
// doesn't work:
// [warn]  ::::::::::::::::::::::::::::::::::::::::::::::
// [warn]  ::          UNRESOLVED DEPENDENCIES         ::
// [warn]  ::::::::::::::::::::::::::::::::::::::::::::::
// [warn]  :: math-with-scala#math-with-scala_2.12;0.1.0: not found
// [warn]  ::::::::::::::::::::::::::::::::::::::::::::::
//
//lazy val docs = project       // new documentation project
//  .in(file("generated-docs")) // important: it must not be docs/
//  .dependsOn(root)
//  .enablePlugins(MdocPlugin)

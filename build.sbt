val Http4sVersion = "0.21.4"

val CirceVersion = "0.13.0"

val Specs2Version = "4.8.0"

val LogbackVersion = "1.2.3"

val catsRetryVersion = "1.1.0"
val catsMtlVersion = "0.7.0"

val meowVesion = "0.3.0-M1"

val monocleVersion = "2.0.0"

lazy val root = (project in file("."))
  .settings(
    organization := "cz.copr",
    name := "chess",
    version := "0.0.1-SNAPSHOT",
    scalaVersion := "2.13.0",
    libraryDependencies ++= Seq(
      "org.http4s"      %% "http4s-blaze-server" % Http4sVersion,
      "org.http4s"      %% "http4s-blaze-client" % Http4sVersion,
      "org.http4s"      %% "http4s-circe"        % Http4sVersion,
      "org.http4s"      %% "http4s-dsl"          % Http4sVersion,
      "io.circe"        %% "circe-generic"       % CirceVersion,
      "org.specs2"      %% "specs2-core"         % Specs2Version % "test",
      "ch.qos.logback"  %  "logback-classic"     % LogbackVersion,

      "com.github.julien-truffaut" %%  "monocle-core"  % monocleVersion,
      "com.github.julien-truffaut" %%  "monocle-macro" % monocleVersion,
      "com.github.julien-truffaut" %%  "monocle-law"   % monocleVersion % "test",

      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
      "dev.profunktor" %% "console4cats" % "0.8.0",
      "io.estatico" %% "newtype" % "0.4.3",
      "eu.timepit" %% "refined" % "0.9.10",
      "com.github.cb372" %% "cats-retry" % "1.0.0",
      "org.typelevel" %% "cats-free" % "2.1.1",
      "com.olegpy" %% "meow-mtl" % meowVesion,
      "com.github.cb372" %% "cats-retry" % catsRetryVersion,
      "org.typelevel" %% "cats-mtl-core" % catsMtlVersion,

"joda-time" % "joda-time" % "2.10.6"
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.10.3"),
    addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.0")
  )


scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-language:higherKinds",
  "-language:postfixOps",
  "-feature",
  "-Xfatal-warnings",
)

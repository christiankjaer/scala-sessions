val scala3Version = "3.2.1-RC1"

val ceVersion = "3.3.14"

lazy val root = project
  .in(file("."))
  .settings(
    name := "sessions",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.8.0",
      "org.typelevel" %% "cats-effect" % ceVersion,
      "org.typelevel" %% "cats-effect-std" % ceVersion,
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )

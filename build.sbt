scalaVersion in ThisBuild := "2.12.2"

lazy val root = project.in(file("."))
  .aggregate(
    mlscalaJVM,
    mlscalaJS
  )
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.0.6",
      "org.scalactic" %% "scalactic" % "3.0.1",
      "org.scalatest" %% "scalatest" % "3.0.1" % "test"
    ),
    scalaJSUseMainModuleInitializer := true
  )

lazy val mlscala = crossProject.crossType(CrossType.Pure)
  .settings(
    name := "mlscala",
    version := "1.0"
  )
  .jvmSettings(
  )
  .jsSettings(
  )

lazy val mlscalaJS = mlscala.js
lazy val mlscalaJVM = mlscala.jvm

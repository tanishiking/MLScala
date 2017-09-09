scalaVersion in ThisBuild := "2.12.2"

lazy val root = project.in(file("."))
  .aggregate(mlscalaJS, mlscalaJVM)

lazy val mlscala = crossProject.crossType(CrossType.Full).in(file("."))
  .settings(
    name := "mlscala",
    version := "1.0",
    // scalaVersion := ScalaVersion,
    libraryDependencies ++= Seq(
      // "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.0.6",
      "com.lihaoyi" %%% "fastparse" % "0.4.3",
      "org.scalactic" %% "scalactic" % "3.0.1",
      "org.scalatest" %% "scalatest" % "3.0.1" % "test"
    )
  )
  .jsSettings()
  .jvmSettings()

lazy val mlscalaJS = mlscala.js
lazy val mlscalaJVM = mlscala.jvm
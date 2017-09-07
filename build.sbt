lazy val root = project.in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "mlscala",
    version := "1.0",
    scalaVersion := "2.12.2",
    libraryDependencies ++= Seq(
      // "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.0.6",
      "com.lihaoyi" %%% "fastparse" % "0.4.3",
      "org.scalactic" %% "scalactic" % "3.0.1",
      "org.scalatest" %% "scalatest" % "3.0.1" % "test"
    )
    // scalaJSUseMainModuleInitializer := true
  )

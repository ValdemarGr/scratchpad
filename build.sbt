ThisBuild / scalaVersion := "3.3.0"

lazy val root = project.in(file("."))
  .settings(
    libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-effect" % "3.5.2",
    "org.typelevel" %% "cats-mtl" % "1.3.1",
    "org.typelevel" %% "cats-core" % "2.9.0",
    "org.typelevel" %% "cats-free" % "2.9.0",
      "co.fs2" %% "fs2-core" % "3.7.0",
      "co.fs2" %% "fs2-io" % "3.7.0",
    "org.typelevel" %% "cats-parse" % "0.3.8",
    "io.circe" %% "circe-core" % "0.14.6",
    "io.circe" %% "circe-parser" % "0.14.6",
    "io.circe" %% "circe-literal" % "0.14.6",
    "org.tpolecat" %% "sourcepos" % "1.1.0",
    "org.typelevel" %% "paiges-core" % "0.4.2",
      "org.typelevel" %% "vault" % "3.5.0",
    "org.scalameta" %% "munit" % "1.0.0-M10" ,
    "org.typelevel" %% "munit-cats-effect" % "2.0.0-M3",
    "com.disneystreaming" %% "weaver-cats" % "0.8.4"
    ),
    testFrameworks += new TestFramework("weaver.framework.CatsEffect")
  )
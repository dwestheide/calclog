val scala3Version = "3.0.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "calclog",
    organization := "com.danielwestheide",
    version := "0.2.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq("io.monix" %% "minitest-laws" % "2.9.6" % "test"),
    testFrameworks += new TestFramework("minitest.runner.Framework"),
    githubOwner := "dwestheide",
    githubRepository := "calclog"
  )

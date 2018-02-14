organization in ThisBuild := "com.github.calclog"

scalaVersion in ThisBuild := "2.12.4"

enablePlugins(GitVersioning)
enablePlugins(GitBranchPrompt)

testFrameworks in ThisBuild += new TestFramework("minitest.runner.Framework")

lazy val calclog = project.in(file("calclog"))
  .withId("calclog")
  .settings(
    name := "calclog",
    libraryDependencies += "io.monix" %% "minitest" % "2.0.0" % "test",
    libraryDependencies += "io.monix" %% "minitest-laws" % "2.0.0" % "test"
  )

lazy val examples = project.in(file("examples"))
  .withId("examples")
  .settings(
    name := "calclog-examples",
    coverageEnabled := false,
    initialCommands in console := "import calclog.CalculationFormatter.syntax._, " +
      "calclog.Implicits._, calclog.ValueFormatter.Implicits._"
  )
  .dependsOn(calclog)

lazy val root = project.in(file("."))
  .withId("calclog-parent")
  .settings(
    name := "calclog-parent"
  ).aggregate(calclog, examples)

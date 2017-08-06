scalaVersion := "2.12.3"

enablePlugins(ScalaJSPlugin)
scalaJSUseMainModuleInitializer := true

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.1",
  "org.scalatest" %% "scalatest" % "3.0.3" % Test,
  "com.github.dwickern" %% "scala-nameof" % "1.0.3" % Provided
)

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-Xlint",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused-import"
)

scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "src"

val testFilePattern = "*.Test.scala"
excludeFilter in Compile := testFilePattern
excludeFilter in Test := NothingFilter
includeFilter in Test := testFilePattern

logBuffered in Test := false

name := "ray-tracing-the-next-week"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

scalacOptions ++= Seq(
  "-target:jvm-1.8",        // Target Java 8
  "-explaintypes",          // Explain type errors with more detail
  "-deprecation",           // Emit deprecation warnings
  "-feature",               // Emit warnings where feature needs explicit import
  "-unchecked",             // Emit warnings related to type erasure
  "-Ywarn-unused:imports",  // Warn on unused imports
  "-Xfatal-warnings"        // Make warnings fatal
)

// Filter options that don't play well with the scala console.
// See https://tpolecat.github.io/2017/04/25/scalac-flags.html
scalacOptions in (Compile, console) ~= (_.filterNot(Set(
  "-Ywarn-unused:imports",
  "-Xfatal-warnings"
)))

assemblyJarName in assembly := "raytracer.jar"
mainClass in assembly := Some("uk.carwynellis.raytracing.Render")
test in assembly := {}

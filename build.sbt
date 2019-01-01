name := "scala-with-cats-exercises"

version := "1.0"

scalaVersion := "2.12.4"

val catsVersion = "1.0.0"

libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core"   % catsVersion,
    "org.typelevel" %% "cats-free"   % catsVersion,
    "org.scalatest" %% "scalatest"   % "3.0.5"      % "test"
)

resolvers += Resolver.sonatypeRepo("releases")

scalacOptions ++= Seq(
  "-target:jvm-1.8",        // Target Java 8
  "-explaintypes",          // Explain type errors with more detail
  "-deprecation",           // Emit deprecation warnings
  "-feature",               // Emit warnings where feature needs explicit import
  "-unchecked",             // Emit warnings related to type erasure
  "-Ywarn-unused:imports",  // Warn on unused imports
  "-Xfatal-warnings",       // Make warnings fatal
  "-Ypartial-unification"   // Improve type constructor inference - see SI-2712
)

// Filter options that don't play well with the scala console.
// See https://tpolecat.github.io/2017/04/25/scalac-flags.html
scalacOptions in (Compile, console) ~= (_.filterNot(Set(
  "-Ywarn-unused:imports",
  "-Xfatal-warnings"
)))

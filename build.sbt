
lazy val commonSettings = Seq(
  organization := "snakefish",
  version := "0.1",
  scalaVersion := "2.11.7",
  resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"
)

lazy val scalacheck = "org.scalacheck" %% "scalacheck" % "1.13.0"
lazy val scalatest  = "org.scalatest"  %% "scalatest"  % "2.2.6"

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "Cryptograph",

    libraryDependencies += scalacheck % Test,

    libraryDependencies += scalatest % Test,

    maxErrors := 20,

    pollInterval := 1000,

    javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),

    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xfatal-warnings"),

    initialCommands := """
      |import System.{currentTimeMillis => now}
      |def time[T](f: => T): T = {
      |  val start = now
      |  try { f } finally { println("Elapsed: " + (now - start)/1000.0 + " s") }
      |}""".stripMargin,

    initialCommands in console := "import snakefish.crypto._",

    shellPrompt in ThisBuild := { state => Project.extract(state).currentRef.project + "> " },

    fork in Test := true,

    javaOptions += "-Xmx2G"
  )

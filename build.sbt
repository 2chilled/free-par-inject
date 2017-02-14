name := "free-par-inject"

organization := "com.github.2chilled"

scalaVersion in ThisBuild := "2.11.8"

scalacOptions ++= Seq(
  "-feature"
)

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.8",
  "org.scalaz" %% "scalaz-concurrent" % "7.2.8",
  "org.log4s" %% "log4s" % "1.3.4"
)

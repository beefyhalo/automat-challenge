scalaVersion := "2.12.7"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.http4s" %% "http4s-blaze-client" % "0.20.0-M2",
  "org.http4s" %% "http4s-circe" % "0.20.0-M2",
  "io.circe" %% "circe-generic" % "0.10.1",
  "io.chrisdavenport" %% "cats-par" % "0.2.0"
)
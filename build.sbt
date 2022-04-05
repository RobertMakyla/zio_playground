name := "zio_playground"

version := "1.0"

scalaVersion := "2.12.0"


libraryDependencies ++= {
  val scalaTestV       = "3.0.1"
  val scalaCheckVersion = "1.13.4"

  Seq(
    "org.scalatest"  %% "scalatest"  % scalaTestV % "test",
    "org.scalacheck" %% "scalacheck" % scalaCheckVersion % "test"
  )
}
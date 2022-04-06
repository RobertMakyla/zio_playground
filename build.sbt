name := "zio_playground"

version := "1.0"

scalaVersion := "2.13.8"

libraryDependencies ++= {
  val zioVersion = "2.0.0-M4"

  Seq(
    "dev.zio" %% "zio" % zioVersion,
    "dev.zio" %% "zio-streams" % zioVersion
  )
}
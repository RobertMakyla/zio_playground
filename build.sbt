name := "zio_playground"

version := "1.0"

scalaVersion := "2.13.8"

libraryDependencies ++= {
  val zioVersion               = "2.0.0-RC5"
  val zioHttpVersion           = "2.0.0-RC7"
  Seq(
    "dev.zio"               %% "zio"                               % zioVersion,
    "io.d11"                %% "zhttp"                             % zioHttpVersion
  )
}
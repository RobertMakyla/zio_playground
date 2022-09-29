name := "zio_playground"

version := "1.0"

scalaVersion := "2.13.8"

libraryDependencies ++= {
  val zioVersion = "2.0.1"          // https://github.com/zio/zio/releases
  val zioHttpVersion = "2.0.0-RC11" // https://github.com/zio/zio-http/releases
  val CatsCoreV = "2.3.0"
  val CatsFreeV = "2.3.0"
  Seq(
    "dev.zio" %% "zio" % zioVersion,
    "io.d11" %% "zhttp" % zioHttpVersion,
    "org.typelevel" %% "cats-core" % CatsCoreV,
    "org.typelevel" %% "cats-kernel" % CatsCoreV,
    "org.typelevel" %% "cats-free" % CatsFreeV,
  )
}
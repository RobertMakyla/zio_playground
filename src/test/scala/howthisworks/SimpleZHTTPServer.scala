package howthisworks

import zhttp.http._
import zhttp.service.Server
import zio._

object SimpleZHTTPServer extends ZIOAppDefault {

  // Create HTTP route
  val app: HttpApp[Any, Nothing] = Http.collect[Request] {
    case Method.GET -> !! / "text" => Response.text("Hello World!")
    case Method.GET -> !! / "json" => Response.json("""{"greetings": "Hello World!"}""")
  }

  // Run it like any simple app
  override val run: ZIO[Any, Throwable, Unit] = Server.start(8090, app)
}
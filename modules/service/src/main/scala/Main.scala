package itc

import edu.gemini.itc.web.servlets.JsonServlet
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.ServletHandler

object Main {

  def main(args: Array[String]): Unit = {

    // Let's not connect to the window server
    System.setProperty("java.awt.headless", "true")

    // Construct a server on `PORT` or 8080 if not provided
    val port    = sys.env.get("PORT").fold(8080)(_.toInt)
    val server  = new Server(port)
    val handler = new ServletHandler()

    // Set up our handler
    handler.addServletWithMapping(classOf[JsonServlet], "/json")

    // And start our server
    server.setHandler(handler)
    server.start()
    server.join()

  }

}
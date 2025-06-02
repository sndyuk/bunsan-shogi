package jp.sndyuk.shogi.web

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.webapp.WebAppContext
import org.scalatra.servlet.ScalatraListener // Corrected import for ScalatraListener

object WebServer {
  def main(args: Array[String]): Unit = {
    val server = new Server(8080) // Port 8080
    val context = new WebAppContext()

    context.setContextPath("/")
    // Set resourceBase to a directory where WEB-INF/web.xml (if any) and static assets would be.
    // For Scalatra with embedded Jetty and programmatic bootstrap, this often points to "src/main/webapp".
    // If you have static assets in "src/main/resources/webroot", Scalatra might serve them automatically
    // or require additional configuration in ShogiWebApp or ScalatraBootstrap.
    context.setResourceBase("src/web/src/main/webapp") 
    context.setInitParameter(ScalatraListener.LifeCycleKey, classOf[ScalatraBootstrap].getName)
    context.addEventListener(new ScalatraListener())

    server.setHandler(context)

    try {
      println("Starting Shogi Web Server on port 8080...")
      server.start()
      println("Server started. Base URL: http://localhost:8080/api/")
      println("Press any key to stop server.")
      System.in.read() // Keep server running
      println("Stopping server...")
      server.stop()
      server.join()
      println("Server stopped.")
    } catch {
      case e: Exception =>
        e.printStackTrace()
        System.exit(1)
    }
  }
}

package jp.sndyuk.shogi.web

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.webapp.WebAppContext
import org.scalatra.servlet.ScalatraListener
import java.net.URL // Added for URL

object WebServer {
  def main(args: Array[String]): Unit = {
    val server = new Server(8080) // Port 8080
    val context = new WebAppContext()

    context.setContextPath("/")

    // Try to find the 'webroot' directory via classloader
    val webrootUrl: Option[URL] = Option(getClass.getClassLoader.getResource("webroot"))

    if (webrootUrl.isEmpty) {
      println("ERROR: Could not find 'webroot' directory in classpath. Static files might not be served correctly.")
      // Fallback to previous direct path, but this might not be reliable in all execution environments (e.g. from JAR)
      println("Falling back to relative path: src/web/src/main/resources/webroot")
      context.setResourceBase("src/web/src/main/resources/webroot") 
    } else {
      val resourceBasePath = webrootUrl.get.toExternalForm
      println(s"Setting resourceBase to: $resourceBasePath (found via classloader)")
      context.setResourceBase(resourceBasePath)
    }
    
    context.setInitParameter(ScalatraListener.LifeCycleKey, "jp.sndyuk.shogi.web.ScalatraBootstrap")
    context.addEventListener(new ScalatraListener())

    server.setHandler(context)

    try {
      println("Starting Shogi Web Server on port 8080...")
      server.start()
      println("Server started. Base URL for API: http://localhost:8080/api/")
      println("Static files (e.g. index.html) should be at: http://localhost:8080/")
      println("Press any key to stop server.")
      System.in.read() // Keep server running
    } catch {
      case e: Exception =>
        e.printStackTrace()
        System.exit(1)
    } finally {
      println("Stopping server...")
      server.stop()
      server.join()
      println("Server stopped.")
    }
  }
}

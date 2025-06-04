package jp.sndyuk.shogi.web // Added package declaration

// import jp.sndyuk.shogi.web.ShogiWebApp // ShogiWebApp is in the same package
import org.scalatra._
import javax.servlet.ServletContext

class ScalatraBootstrap extends LifeCycle {
  override def init(context: ServletContext): Unit = {
    // Mount servlets.
    context.mount(new ShogiWebApp, "/api/*") // Mount ShogiWebApp under /api
    println("ScalatraBootstrap: init method called, ShogiWebApp mounted.")
  }

  override def destroy(context: ServletContext): Unit = {
    println("ScalatraBootstrap: destroy method called.")
    super.destroy(context)
  }
}

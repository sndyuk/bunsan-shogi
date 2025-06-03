package jp.sndyuk.shogi.web

import org.scalatra._
// GameState is used for Json.toJson(gameState), Position for toCorePosition, ShogiGameService is instantiated, SimplePiece for drop type matching
import jp.sndyuk.shogi.core.{GameState, Position, ShogiGameService, SimplePiece}
import play.api.libs.json.{Json, Format, JsValue, Writes} // Play JSON imports

// --- JSON Case Classes for API Requests ---
case class WebPosition(x: Int, y: Int)
object WebPosition {
  implicit val format: Format[WebPosition] = Json.format[WebPosition]
  def toCorePosition(wp: WebPosition): Position = Position(wp.x, wp.y)
  def fromCorePosition(cp: Position): WebPosition = WebPosition(cp.x, cp.y)
}

// For board moves
case class BoardMoveRequest(from: WebPosition, to: WebPosition, promotion: Boolean)
object BoardMoveRequest {
  implicit val format: Format[BoardMoveRequest] = Json.format[BoardMoveRequest]
}

// For drops
case class DropMoveRequest(to: WebPosition, droppedPiece: String) // piece as String like "FU", "KA"
object DropMoveRequest {
  implicit val format: Format[DropMoveRequest] = Json.format[DropMoveRequest]
}

// --- Implicit Play JSON Writers for Core Types (if not already globally available) ---
// GameState.scala already defines formats for Position, SimplePiece.SimplePieceType, Player.Player, GameState
// So, they should be in scope if GameState itself is serializable.
// We might need a specific Writes for List[Position] if default doesn't work as expected by client.
object CoreTypeFormats {
  // Position format is in GameState.scala's Position companion object
  // SimplePieceType format is in GameState.scala's SimplePiece companion object
  // Player format is in GameState.scala's Player companion object
  // GameState format is in GameState.scala's GameState companion object

  // For List[Position] - default Play JSON list writer should work if Position has a format.
  implicit val listPositionWrites: Writes[List[Position]] = Writes.list[Position](Position.positionJsonFormat)

}


class ShogiWebApp extends ScalatraServlet {

  // Instantiate the game service
  val shogiGameService = new ShogiGameService()

  // Import a marshaller for Play JSON
  // For Scalatra, you might need a specific PlayJsonSupport trait or handle manually
  // Manual handling:
  before() {
    contentType = "application/json"
  }

  import CoreTypeFormats._ // Make implicit writers available

  // --- API Endpoints ---

  // GET /game/state
  get("/game/state") {
    val gameState: GameState = shogiGameService.getGameState() // Explicit type
    Json.toJson(gameState).toString()
  }

  // POST /game/new
  post("/game/new") {
    // For now, starts a default new game. Could be extended to take parameters.
    val newGameState: GameState = shogiGameService.startNewGame() // Explicit type
    Json.toJson(newGameState).toString()
  }

  // GET /game/valid_moves?x=:x&y=:y
  get("/game/valid_moves") {
    val xParam = params.getAs[Int]("x")
    val yParam = params.getAs[Int]("y")

    (xParam, yParam) match {
      case (Some(x), Some(y)) =>
        try {
          val fromPos = Position(x,y) // This is jp.sndyuk.shogi.core.Position
          val validMoves = shogiGameService.getValidMoves(fromPos) // Returns List[core.Position]
          Json.toJson(validMoves).toString() // Uses implicit listPositionWrites
        } catch {
          case e: Exception =>
            halt(BadRequest(Json.obj("error" -> s"Error processing valid_moves: ${e.getMessage}").toString()))
        }
      case _ =>
        halt(BadRequest(Json.obj("error" -> "Missing or invalid x, y query parameters for from_pos").toString()))
    }
  }

  // POST /game/move
  post("/game/move") {
    val body = request.body
    val jsonBody: JsValue = try {
      Json.parse(body)
    } catch {
      case e: Exception => halt(BadRequest(Json.obj("error" -> s"Invalid JSON body: ${e.getMessage}").toString()))
    }

    // Try to parse as BoardMoveRequest
    jsonBody.validate[BoardMoveRequest].asOpt match {
      case Some(boardMove) =>
        val fromPos = WebPosition.toCorePosition(boardMove.from)
        val toPos = WebPosition.toCorePosition(boardMove.to)
        shogiGameService.makeMove(fromPos, toPos, boardMove.promotion, None) match {
          case Right(gs: GameState) => Json.toJson(gs).toString() // Explicit type
          case Left(errorMsg)   => BadRequest(Json.obj("error" -> errorMsg).toString())
        }
      case None =>
        // Try to parse as DropMoveRequest
        jsonBody.validate[DropMoveRequest].asOpt match {
          case Some(dropMove) =>
            val toPos = WebPosition.toCorePosition(dropMove.to)
            try {
              // Convert droppedPiece string to SimplePiece.SimplePieceType
              // Assuming piece names are like "FU", "KA" etc. as in SimplePiece enum
              val pieceType = SimplePiece.withName(dropMove.droppedPiece.toUpperCase)

              // For drops, fromPos is not used by service's makeMove logic if piece type is given
              // However, makeMove signature expects a fromPos. We can use a dummy or conventional one.
              // ShogiGameService's makeMove uses Point.ofCaptured for drops if fromPos indicates a hand piece.
              // Here, we are directly specifying droppedPieceType, so fromPos is less critical for that path.
              // Let's use a dummy Position like (-1,-1) as it's not a valid board square.
              val dummyFromPos = Position(-1, -1)

              shogiGameService.makeMove(dummyFromPos, toPos, promotion = false, Some(pieceType)) match {
                case Right(gs: GameState) => Json.toJson(gs).toString() // Explicit type
                case Left(errorMsg)   => BadRequest(Json.obj("error" -> errorMsg).toString())
              }
            } catch {
              case e: NoSuchElementException => // From SimplePiece.withName if piece string is invalid
                BadRequest(Json.obj("error" -> s"Invalid piece name for drop: ${dropMove.droppedPiece}").toString())
              case e: Exception =>
                InternalServerError(Json.obj("error" -> s"Error processing drop move: ${e.getMessage}").toString())
            }
          case None =>
            BadRequest(Json.obj("error" -> "Invalid request format. Expected BoardMoveRequest or DropMoveRequest.").toString())
        }
    }
  }

  // Error handling for general errors or routes not found
  notFound {
    contentType = "application/json"
    NotFound(Json.obj("error" -> "Route not found").toString())
  }

  error {
    case e: Exception =>
      contentType = "application/json"
      // Log the error server-side as well
      // logger.error(s"Unhandled exception: ${e.getMessage}", e)
      InternalServerError(Json.obj("error" -> "An unexpected error occurred", "detail" -> e.getMessage()).toString())
  }

}

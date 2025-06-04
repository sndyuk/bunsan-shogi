package jp.sndyuk.shogi.web

import org.scalatra._
// GameState is used for Json.toJson(gameState), Position for toCorePosition, ShogiGameService is instantiated, SimplePiece for drop type matching
// GameState, Position, ShogiGameService, SimplePiece are used.
// SimpleTransition type will be fully qualified. Its JSON formatter is imported explicitly.
import jp.sndyuk.shogi.core.{GameState, Position, ShogiGameService, SimplePiece}
import play.api.libs.json.{Json, Format, JsValue, Writes} // Play JSON imports

// --- JSON Case Classes for API Requests ---
case class WebPosition(x: Int, y: Int)
object WebPosition {
  implicit val format: Format[WebPosition] = Json.format[WebPosition]
  // wp.x is 0-8 file index from app.js (0=USI 9, 8=USI 1)
  // wp.y is 0-8 rank index from app.js (0=USI 1, 8=USI 9)
  def toCorePosition(wp: WebPosition): Position = Position(x = 9 - wp.x, y = wp.y + 1)
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

// For new game with options
case class NewGameRequest(gameMode: Option[String], aiType: Option[String], aiSearchDepth: Option[Int])
object NewGameRequest {
  implicit val format: Format[NewGameRequest] = Json.format[NewGameRequest]
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

  // --- API Endpoints ---

  // GET /game/state
  get("/game/state") {
    val gameState: GameState = shogiGameService.getGameState() // Explicit type
    Json.toJson(gameState).toString()
  }

  // POST /game/new
  post("/game/new") {
    val body = request.body
    if (body.trim.isEmpty) {
      // No body, start a default game
      val newGameState = shogiGameService.startNewGame(gameMode = "hvh", aiType = "v4", aiSearchDepth = 3) // Default HVH
      Json.toJson(newGameState).toString()
    } else {
      // Body present, try to parse as NewGameRequest
      val jsonBody: JsValue = try {
        Json.parse(body)
      } catch {
        case e: Exception => halt(BadRequest(Json.obj("error" -> s"Invalid JSON body: ${e.getMessage}").toString()))
      }

      jsonBody.validate[NewGameRequest].asOpt match {
        case Some(req) =>
          val gameMode = req.gameMode.getOrElse("hvh")
          val aiType = req.aiType.getOrElse("v4")
          val aiSearchDepth = req.aiSearchDepth.getOrElse(3)

          val newGameState = shogiGameService.startNewGame(
            // initialBoardSetup, initialSenteCaptured, initialGoteCaptured, firstPlayer will use defaults in service
            gameMode = gameMode,
            aiType = aiType,
            aiSearchDepth = aiSearchDepth
          )
          Json.toJson(newGameState).toString()
        case None =>
          BadRequest(Json.obj("error" -> "Invalid request format for new game. Expected NewGameRequest or empty body for default.").toString())
      }
    }
  }

  // GET /game/suggest_move
  get("/game/suggest_move") {
    val aiTypeParam = params.get("aiType").getOrElse("v4")
    val aiSearchDepthParam = params.getAs[Int]("aiSearchDepth").getOrElse(3)

    // This method shogiGameService.suggestMove(aiType, depth) needs to be implemented in ShogiGameService
    // It should return Either[String, SimpleTransition]
    shogiGameService.suggestMove(aiTypeParam, aiSearchDepthParam) match {
      // The type of suggestionData is now Map[String, JsValue]
      case Right(suggestionData) => Json.toJson(suggestionData).toString() // MODIFIED LINE
      case Left(errorMsg)     => BadRequest(Json.obj("error" -> errorMsg).toString()) // No change
    }
  }

  // POST /game/ai_move
  post("/game/ai_move") {
    shogiGameService.requestAIMove() match {
      case Right(gameState) => Json.toJson(gameState).toString()
      case Left(errorMsg)   => BadRequest(Json.obj("error" -> errorMsg).toString())
    }
  }

  // GET /game/valid_moves?x=:x&y=:y
  get("/game/valid_moves") {
    val xParam = params.getAs[Int]("x")
    val yParam = params.getAs[Int]("y")

    (xParam, yParam) match {
      case (Some(x), Some(y)) =>
        // xParam (x) is 0-8 file index from app.js (0=USI 9, 8=USI 1)
        // yParam (y) is 0-8 rank index from app.js (0=USI 1, 8=USI 9)
        val usiFile = 9 - x
        val usiRank = y + 1
        try {
          val fromPos = Position(usiFile, usiRank) // jp.sndyuk.shogi.core.Position
          val validCorePositions = shogiGameService.getValidMoves(fromPos) // This is List[jp.sndyuk.shogi.core.Position]

          // NEW: Transform to List[WebPosition]
          val validWebPositions = validCorePositions.map { corePos =>
            WebPosition(x = 9 - corePos.x, y = corePos.y - 1)
          }
          Json.toJson(validWebPositions).toString() // Serialize the transformed list
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

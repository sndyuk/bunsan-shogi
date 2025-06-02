package jp.sndyuk.shogi.core

import jp.sndyuk.shogi.core.Piece.Piece
import jp.sndyuk.shogi.core.Player.Player
import play.api.libs.json._
import play.api.libs.functional.syntax._ // For custom Reads/Writes using functional syntax

import java.io.{File, PrintWriter}
import scala.io.Source
import scala.util.{Failure, Success, Try, Using}

// Minimal Player enum (assuming SENTE/GOTE)
object Player extends Enumeration {
  type Player = Value
  val SENTE, GOTE = Value

  implicit val playerFormat: Format[Player] = new Format[Player] {
    def reads(json: JsValue): JsResult[Player] = json.validate[String].flatMap { s =>
      Try(Player.withName(s)).map(JsSuccess(_)).getOrElse(JsError(s"Unknown Player: $s"))
    }
    def writes(player: Player): JsValue = JsString(player.toString)
  }
}

// Minimal Position case class
case class Position(x: Int, y: Int)

object Position {
  // Format for Position when it's a standalone object or value
  implicit val positionJsonFormat: Format[Position] = Json.format[Position]

  // For using Position as a key in a Map[Position, Piece]
  // We need to read/write it as a String key in JSON.
  // Example: {"1,2": "KING", "3,4": "PAWN"}
  implicit val positionMapKeyReads: KeyReads[Position] = (key: String) => {
    key.split(',').map(_.trim) match {
      case Array(xStr, yStr) =>
        Try(Position(xStr.toInt, yStr.toInt))
          .map(JsSuccess(_))
          .getOrElse(JsError(s"Invalid Position string for map key: $key"))
      case _ => JsError(s"Invalid Position string for map key: $key")
    }
  }
  implicit val positionMapKeyWrites: KeyWrites[Position] = (pos: Position) => s"${pos.x},${pos.y}"
}

// Minimal Piece enum (example pieces)
object Piece extends Enumeration {
  type Piece = Value
  val KING, ROOK, BISHOP, GOLD, SILVER, KNIGHT, LANCE, PAWN = Value

  implicit val pieceFormat: Format[Piece] = new Format[Piece] {
    def reads(json: JsValue): JsResult[Piece] = json.validate[String].flatMap { s =>
      Try(Piece.withName(s)).map(JsSuccess(_)).getOrElse(JsError(s"Unknown Piece: $s"))
    }
    def writes(piece: Piece): JsValue = JsString(piece.toString)
  }
}

// Minimal Transition class
case class Transition(move: String, boardStateAfterMove: Map[Position, Piece])

object Transition {
  // This will pick up Position.positionMapKeyReads/Writes for the Map keys
  // and Piece.pieceFormat for the Map values.
  implicit val transitionFormat: Format[Transition] = Json.format[Transition]
}


case class GameState(
    boardSetup: Map[Position, Piece], // Pieces and their positions
    currentTurn: Player, // Player whose turn it is
    capturedPiecesPlayer1: List[Piece], // Captured pieces by Player 1
    capturedPiecesPlayer2: List[Piece], // Captured pieces by Player 2
    gameHistory: List[Transition] // List of moves or Transition objects
)

object GameState {
  // This will also pick up Position.positionMapKeyReads/Writes and Piece.pieceFormat
  implicit val gameStateFormat: Format[GameState] = Json.format[GameState]
}

object GameSaver {

  def saveToFile(gameState: GameState, filePath: String): Try[Unit] = {
    Try {
      val jsonString = Json.prettyPrint(Json.toJson(gameState))
      Using(new PrintWriter(new File(filePath))) { writer =>
        writer.write(jsonString)
      }.get // .get will rethrow exception from Using if any, or return Unit
    }
  }

  def loadFromFile(filePath: String): Try[GameState] = {
    Try {
      Using(Source.fromFile(filePath)) { source =>
        val jsonString = source.mkString
        Json.parse(jsonString).validate[GameState] match {
          case JsSuccess(gs, _) => gs
          case JsError(errors) =>
            throw new RuntimeException(s"Failed to parse GameState from JSON: ${errors.mkString(", ")}")
        }
      }.get // .get will rethrow exception from Using if any, or return the GameState
    }
  }
}

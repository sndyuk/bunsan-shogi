package jp.sndyuk.shogi.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}
// Removed scala.util.{Try, Success, Failure} imports

// GameState, GameSaver, Player, SimplePiece, Position, SimpleTransition are in the same package (jp.sndyuk.shogi.core)
// No explicit imports needed for them.
// Alias SimplePieceType to PieceValue for use in Map value types
import jp.sndyuk.shogi.core.SimplePiece.{SimplePieceType => PieceValue, _}
// Alias SimpleTransition if it helps clarity, though it's also directly accessible.
import jp.sndyuk.shogi.core.{SimpleTransition => SavedTransition}


class GameSaverSpec extends AnyFlatSpec with Matchers {

  // Helper to create a temporary file
  def withTempFile(testCode: Path => Any): Unit = {
    val tempFile = Files.createTempFile("gameSaverSpec", ".json")
    try {
      testCode(tempFile)
    } finally {
      Files.deleteIfExists(tempFile)
    }
  }

  // Sample data for board setup
  val sampleBoardSetup: Map[Position, PieceValue] = Map( // Use PieceValue for Map value type
    Position(1, 1) -> LANCE, // Use LANCE directly from SimplePiece import
    Position(1, 2) -> KNIGHT,
    Position(5, 5) -> KING
  )

  val sampleBoardSetupMidGame: Map[Position, PieceValue] = Map( // Use PieceValue
    Position(7, 6) -> PAWN, // Sente's pawn advanced
    Position(3, 4) -> PAWN, // Gote's pawn advanced
    Position(5, 8) -> KING, // Sente King
    Position(5, 2) -> KING, // Gote King
    Position(2, 2) -> ROOK, // Sente Rook
    Position(8, 8) -> BISHOP // Gote Bishop
  )

  // Sample game history
  // Recall SavedTransition is (move: String, boardStateAfterMove: Map[Position, PieceValue])
  val sampleHistory: List[SavedTransition] = List(
    SavedTransition("7g7f", Map(Position(7,6) -> PAWN) ++ sampleBoardSetup - Position(7,7)), // Pawn from 77 to 76
    SavedTransition("3c3d", Map(Position(3,4) -> PAWN) ++ sampleBoardSetup - Position(3,3) - Position(7,7) + (Position(7,6) -> PAWN))
  )

  val emptyHistory: List[SavedTransition] = Nil

  "GameSaver" should "save and load a simple game state correctly" in withTempFile { filePath =>
    val originalGameState = GameState(
      boardSetup = sampleBoardSetup,
      currentTurn = Player.SENTE,
      capturedPiecesPlayer1 = List(BISHOP, PAWN), // Use direct piece names
      capturedPiecesPlayer2 = List(ROOK),
      gameHistory = sampleHistory
    )

    val saveResult = GameSaver.saveToFile(originalGameState, filePath.toString)
    saveResult should be a 'success // Check if Try is Success

    val loadResult = GameSaver.loadFromFile(filePath.toString)
    loadResult should be a 'success

    val loadedGameState = loadResult.get
    loadedGameState shouldEqual originalGameState
  }

  it should "save and load an initial game state" in withTempFile { filePath =>
    // Representing an initial "hirate" setup is complex for `boardSetup` which expects generic Pieces.
    // For this test, "initial" means Sente's turn, no captures, no history.
    // A full board setup would involve all initial pieces.
    val initialBoard: Map[Position, PieceValue] = Map( // Simplified initial setup; Use PieceValue
      Position(1,1) -> LANCE, Position(2,1) -> KNIGHT, /* ... Sente pieces ... */
      Position(9,9) -> LANCE, Position(8,9) -> KNIGHT  /* ... Gote pieces ... */
      // This should ideally map all standard shogi starting pieces
    )
    val originalGameState = GameState(
      boardSetup = initialBoard, // Placeholder for a more complete initial board
      currentTurn = Player.SENTE,
      capturedPiecesPlayer1 = Nil,
      capturedPiecesPlayer2 = Nil,
      gameHistory = emptyHistory
    )

    GameSaver.saveToFile(originalGameState, filePath.toString) should be a 'success
    val loadedGameState = GameSaver.loadFromFile(filePath.toString).get
    loadedGameState shouldEqual originalGameState
  }

  it should "save and load a mid-game state with more complex data" in withTempFile { filePath =>
    val complexHistory = List(
        SavedTransition("7g7f", sampleBoardSetupMidGame - Position(7,7) + (Position(7,6) -> PAWN)),
        SavedTransition("3c3d", sampleBoardSetupMidGame - Position(7,7) + (Position(7,6) -> PAWN) - Position(3,3) + (Position(3,4) -> PAWN)),
        SavedTransition("2h7h", sampleBoardSetupMidGame - Position(7,7) + (Position(7,6) -> PAWN) - Position(3,3) + (Position(3,4) -> PAWN) - Position(2,8) + (Position(7,8) -> BISHOP)) // Bishop move
    )
    val originalGameState = GameState(
      boardSetup = sampleBoardSetupMidGame,
      currentTurn = Player.GOTE,
      capturedPiecesPlayer1 = List(PAWN, PAWN, LANCE),
      capturedPiecesPlayer2 = List(SILVER),
      gameHistory = complexHistory
    )
    GameSaver.saveToFile(originalGameState, filePath.toString) should be a 'success
    val loadedGameState = GameSaver.loadFromFile(filePath.toString).get
    loadedGameState shouldEqual originalGameState
  }

  it should "save and load a game state with empty history" in withTempFile { filePath =>
    val originalGameState = GameState(
      boardSetup = sampleBoardSetup,
      currentTurn = Player.SENTE,
      capturedPiecesPlayer1 = List(GOLD),
      capturedPiecesPlayer2 = Nil,
      gameHistory = emptyHistory
    )
    GameSaver.saveToFile(originalGameState, filePath.toString) should be a 'success
    val loadedGameState = GameSaver.loadFromFile(filePath.toString).get
    loadedGameState shouldEqual originalGameState
  }

  it should "save and load with captured pieces for both players" in withTempFile { filePath =>
    val originalGameState = GameState(
      boardSetup = Map(Position(5,5) -> KING), // Minimal board
      currentTurn = Player.GOTE,
      capturedPiecesPlayer1 = List(ROOK, BISHOP, PAWN, PAWN),
      capturedPiecesPlayer2 = List(GOLD, SILVER, KNIGHT, LANCE),
      gameHistory = sampleHistory
    )
    GameSaver.saveToFile(originalGameState, filePath.toString) should be a 'success
    val loadedGameState = GameSaver.loadFromFile(filePath.toString).get
    loadedGameState shouldEqual originalGameState
  }

  it should "return Failure when loading a non-existent file" in {
    val loadResult = GameSaver.loadFromFile("nonExistentFile.json")
    loadResult should be a 'failure
  }

  it should "return Failure when loading a malformed JSON file" in withTempFile { filePath =>
    Files.write(filePath, "this is not json".getBytes)
    val loadResult = GameSaver.loadFromFile(filePath.toString)
    loadResult should be a 'failure
    // Specific error type/message could be asserted if needed, e.g. RuntimeException from GameState.scala
  }
}

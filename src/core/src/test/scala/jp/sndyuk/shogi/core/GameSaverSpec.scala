package jp.sndyuk.shogi.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}
import scala.util.Success

// Ensure these are the types defined in jp.sndyuk.shogi.core.GameState.scala
import jp.sndyuk.shogi.core.{GameState, GameSaver}
import jp.sndyuk.shogi.core.Player // Assuming this is jp.sndyuk.shogi.core.Player defined in GameState.scala
import jp.sndyuk.shogi.core.Piece // Assuming this is jp.sndyuk.shogi.core.Piece defined in GameState.scala
import jp.sndyuk.shogi.core.Position // Assuming this is jp.sndyuk.shogi.core.Position defined in GameState.scala
import jp.sndyuk.shogi.core.{Transition => SavedTransition} // Alias to avoid confusion with core.Transition


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
  val sampleBoardSetup: Map[Position, Piece.Value] = Map(
    Position(1, 1) -> Piece.LANCE,
    Position(1, 2) -> Piece.KNIGHT,
    Position(5, 5) -> Piece.KING
  )

  val sampleBoardSetupMidGame: Map[Position, Piece.Value] = Map(
    Position(7, 6) -> Piece.PAWN, // Sente's pawn advanced
    Position(3, 4) -> Piece.PAWN, // Gote's pawn advanced
    Position(5, 8) -> Piece.KING, // Sente King
    Position(5, 2) -> Piece.KING, // Gote King
    Position(2, 2) -> Piece.ROOK, // Sente Rook
    Position(8, 8) -> Piece.BISHOP // Gote Bishop
  )

  // Sample game history
  // Recall SavedTransition is (move: String, boardStateAfterMove: Map[Position, Piece])
  val sampleHistory: List[SavedTransition] = List(
    SavedTransition("7g7f", Map(Position(7,6) -> Piece.PAWN) ++ sampleBoardSetup - Position(7,7)), // Pawn from 77 to 76 (using shogi notation for string)
    SavedTransition("3c3d", Map(Position(3,4) -> Piece.PAWN) ++ sampleBoardSetup - Position(3,3) - Position(7,7) + (Position(7,6) -> Piece.PAWN))
  )

  val emptyHistory: List[SavedTransition] = Nil

  "GameSaver" should "save and load a simple game state correctly" in withTempFile { filePath =>
    val originalGameState = GameState(
      boardSetup = sampleBoardSetup,
      currentTurn = Player.SENTE,
      capturedPiecesPlayer1 = List(Piece.BISHOP, Piece.PAWN),
      capturedPiecesPlayer2 = List(Piece.ROOK),
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
    val initialBoard: Map[Position, Piece.Value] = Map( // Simplified initial setup
      Position(1,1) -> Piece.LANCE, Position(2,1) -> Piece.KNIGHT, /* ... Sente pieces ... */
      Position(9,9) -> Piece.LANCE, Position(8,9) -> Piece.KNIGHT  /* ... Gote pieces ... */
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
        SavedTransition("7g7f", sampleBoardSetupMidGame - Position(7,7) + (Position(7,6) -> Piece.PAWN)),
        SavedTransition("3c3d", sampleBoardSetupMidGame - Position(7,7) + (Position(7,6) -> Piece.PAWN) - Position(3,3) + (Position(3,4) -> Piece.PAWN)),
        SavedTransition("2h7h", sampleBoardSetupMidGame - Position(7,7) + (Position(7,6) -> Piece.PAWN) - Position(3,3) + (Position(3,4) -> Piece.PAWN) - Position(2,8) + (Position(7,8) -> Piece.BISHOP)) // Bishop move (example)
    )
    val originalGameState = GameState(
      boardSetup = sampleBoardSetupMidGame,
      currentTurn = Player.GOTE,
      capturedPiecesPlayer1 = List(Piece.PAWN, Piece.PAWN, Piece.LANCE),
      capturedPiecesPlayer2 = List(Piece.SILVER),
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
      capturedPiecesPlayer1 = List(Piece.GOLD),
      capturedPiecesPlayer2 = Nil,
      gameHistory = emptyHistory
    )
    GameSaver.saveToFile(originalGameState, filePath.toString) should be a 'success
    val loadedGameState = GameSaver.loadFromFile(filePath.toString).get
    loadedGameState shouldEqual originalGameState
  }

  it should "save and load with captured pieces for both players" in withTempFile { filePath =>
    val originalGameState = GameState(
      boardSetup = Map(Position(5,5) -> Piece.KING), // Minimal board
      currentTurn = Player.GOTE,
      capturedPiecesPlayer1 = List(Piece.ROOK, Piece.BISHOP, Piece.PAWN, Piece.PAWN),
      capturedPiecesPlayer2 = List(Piece.GOLD, Piece.SILVER, Piece.KNIGHT, Piece.LANCE),
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

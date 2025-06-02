package jp.sndyuk.shogi.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}
import jp.sndyuk.shogi.core.SimplePiece.{SimplePieceType => PieceValue}
import jp.sndyuk.shogi.core.{SimpleTransition => SavedTransition}


class GameSaverSpec extends AnyFlatSpec with Matchers {

  def withTempFile(testCode: Path => Any): Unit = {
    val tempFile = Files.createTempFile("gameSaverSpec", ".json")
    try {
      testCode(tempFile)
    } finally {
      Files.deleteIfExists(tempFile)
    }
  }

  // Sample data for board setup using correct Shogi enum values
  val sampleBoardSetup: Map[Position, PieceValue] = Map(
    Position(1, 1) -> SimplePiece.KY, // LANCE -> KY
    Position(1, 2) -> SimplePiece.KE, // KNIGHT -> KE
    Position(5, 5) -> SimplePiece.OU  // KING -> OU
  )

  val sampleBoardSetupMidGame: Map[Position, PieceValue] = Map(
    Position(7, 6) -> SimplePiece.FU, // PAWN -> FU
    Position(3, 4) -> SimplePiece.FU, // PAWN -> FU
    Position(5, 8) -> SimplePiece.OU, // KING -> OU (Sente King)
    Position(5, 2) -> SimplePiece.OU, // KING -> OU (Gote King)
    Position(2, 2) -> SimplePiece.HI, // ROOK -> HI
    Position(8, 8) -> SimplePiece.KA  // BISHOP -> KA
  )

  val sampleHistory: List[SavedTransition] = List(
    SavedTransition("7g7f", Map(Position(7,6) -> SimplePiece.FU) ++ sampleBoardSetup - Position(7,7)),
    SavedTransition("3c3d", Map(Position(3,4) -> SimplePiece.FU) ++ sampleBoardSetup - Position(3,3) - Position(7,7) + (Position(7,6) -> SimplePiece.FU))
  )

  val emptyHistory: List[SavedTransition] = Nil

  "GameSaver" should "save and load a simple game state correctly" in withTempFile { filePath =>
    val originalGameState = GameState(
      boardSetup = sampleBoardSetup,
      currentTurn = Player.SENTE,
      capturedPiecesPlayer1 = List(SimplePiece.KA, SimplePiece.FU), // BISHOP -> KA, PAWN -> FU
      capturedPiecesPlayer2 = List(SimplePiece.HI),                 // ROOK -> HI
      gameHistory = sampleHistory
    )

    val saveResult = GameSaver.saveToFile(originalGameState, filePath.toString)
    saveResult should be a 'success

    val loadResult = GameSaver.loadFromFile(filePath.toString)
    loadResult should be a 'success

    val loadedGameState = loadResult.get
    loadedGameState shouldEqual originalGameState
  }

  it should "save and load an initial game state" in withTempFile { filePath =>
    val initialBoard: Map[Position, PieceValue] = Map(
      Position(1,1) -> SimplePiece.KY, Position(2,1) -> SimplePiece.KE, // LANCE -> KY, KNIGHT -> KE
      Position(9,9) -> SimplePiece.KY, Position(8,9) -> SimplePiece.KE  // LANCE -> KY, KNIGHT -> KE
    )
    val originalGameState = GameState(
      boardSetup = initialBoard,
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
        SavedTransition("7g7f", sampleBoardSetupMidGame - Position(7,7) + (Position(7,6) -> SimplePiece.FU)),
        SavedTransition("3c3d", sampleBoardSetupMidGame - Position(7,7) + (Position(7,6) -> SimplePiece.FU) - Position(3,3) + (Position(3,4) -> SimplePiece.FU)),
        SavedTransition("2h7h", sampleBoardSetupMidGame - Position(7,7) + (Position(7,6) -> SimplePiece.FU) - Position(3,3) + (Position(3,4) -> SimplePiece.FU) - Position(2,8) + (Position(7,8) -> SimplePiece.KA)) // BISHOP -> KA
    )
    val originalGameState = GameState(
      boardSetup = sampleBoardSetupMidGame,
      currentTurn = Player.GOTE,
      capturedPiecesPlayer1 = List(SimplePiece.FU, SimplePiece.FU, SimplePiece.KY), // PAWN -> FU, LANCE -> KY
      capturedPiecesPlayer2 = List(SimplePiece.GI),                               // SILVER -> GI
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
      capturedPiecesPlayer1 = List(SimplePiece.KI), // GOLD -> KI
      capturedPiecesPlayer2 = Nil,
      gameHistory = emptyHistory
    )
    GameSaver.saveToFile(originalGameState, filePath.toString) should be a 'success
    val loadedGameState = GameSaver.loadFromFile(filePath.toString).get
    loadedGameState shouldEqual originalGameState
  }

  it should "save and load with captured pieces for both players" in withTempFile { filePath =>
    val originalGameState = GameState(
      boardSetup = Map(Position(5,5) -> SimplePiece.OU), // KING -> OU
      currentTurn = Player.GOTE,
      capturedPiecesPlayer1 = List(SimplePiece.HI, SimplePiece.KA, SimplePiece.FU, SimplePiece.FU), // ROOK->HI, BISHOP->KA, PAWN->FU
      capturedPiecesPlayer2 = List(SimplePiece.KI, SimplePiece.GI, SimplePiece.KE, SimplePiece.KY), // GOLD->KI, SILVER->GI, KNIGHT->KE, LANCE->KY
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
  }
}

package jp.sndyuk.shogi.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}
import jp.sndyuk.shogi.core.SimplePiece.SimplePieceType
// PieceInfo is now part of GameState, so if GameState is imported, PieceInfo should be accessible.
// Or import jp.sndyuk.shogi.core.GameState.PieceInfo if needed, assuming it's defined in GameState object
// For SavedTransition, boardStateAfterMove now uses Map[String, SimplePieceType] as per recent changes
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

  // Helper to convert Position (1-indexed) to "x_y" string key (0-indexed core Point x,y)
  private def posToKey(pos: Position): String = s"${9 - pos.x}_${pos.y - 1}"

  // Sample data for board setup using SimplePieceType
  val sampleBoardSetup: Map[String, SimplePieceType] = Map(
    posToKey(Position(1, 1)) -> SimplePiece.KY,
    posToKey(Position(1, 2)) -> SimplePiece.KE,
    posToKey(Position(5, 5)) -> SimplePiece.OU
  )

  val sampleBoardSetupMidGame: Map[String, SimplePieceType] = Map(
    posToKey(Position(7, 6)) -> SimplePiece.FU,
    posToKey(Position(3, 4)) -> SimplePiece.FU,
    posToKey(Position(5, 8)) -> SimplePiece.OU,
    posToKey(Position(5, 2)) -> SimplePiece.OU,
    posToKey(Position(2, 2)) -> SimplePiece.HI,
    posToKey(Position(8, 8)) -> SimplePiece.KA
  )

  // For history, boardStateAfterMove now expects Map[String, SimplePieceType]
  val sampleHistory: List[SavedTransition] = List(
    SavedTransition("7g7f", sampleBoardSetupMidGame - posToKey(Position(7,7)) + (posToKey(Position(7,6)) -> SimplePiece.FU)),
    SavedTransition("3c3d", sampleBoardSetupMidGame - posToKey(Position(7,7)) + (posToKey(Position(7,6)) -> SimplePiece.FU) - posToKey(Position(3,3)) + (posToKey(Position(3,4)) -> SimplePiece.FU))
  )

  val emptyHistory: List[SavedTransition] = Nil

  "GameSaver" should "save and load a simple game state correctly" in withTempFile { filePath =>
    val originalGameState = GameState(
      boardSetup = sampleBoardSetup,
      currentTurn = Player.SENTE,
      capturedPiecesPlayer1 = List(SimplePiece.KA, SimplePiece.FU),
      capturedPiecesPlayer2 = List(SimplePiece.HI),
      gameHistory = sampleHistory
    )

    val saveResult = GameSaver.saveToFile(originalGameState, filePath.toString)
    saveResult should be a 'success

    val loadResult = GameSaver.loadFromFile(filePath.toString)
    loadResult should be a 'success

    val loadedGameState = loadResult.get
    // Custom equality check if default .equals for Map[String, PieceInfo] is problematic or if order matters anywhere (though Maps are unordered)
    loadedGameState.boardSetup.keys should contain theSameElementsAs originalGameState.boardSetup.keys
    originalGameState.boardSetup.keys.foreach { k =>
        loadedGameState.boardSetup(k) shouldEqual originalGameState.boardSetup(k)
    }
    loadedGameState.copy(boardSetup = Map.empty) shouldEqual originalGameState.copy(boardSetup = Map.empty)
  }

  it should "save and load an initial game state" in withTempFile { filePath =>
    val initialBoard: Map[String, SimplePieceType] = Map(
      posToKey(Position(1,1)) -> SimplePiece.KY, posToKey(Position(2,1)) -> SimplePiece.KE,
      posToKey(Position(9,9)) -> SimplePiece.KY, posToKey(Position(8,9)) -> SimplePiece.KE
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
    loadedGameState.boardSetup.keys should contain theSameElementsAs originalGameState.boardSetup.keys
    originalGameState.boardSetup.keys.foreach { k =>
        loadedGameState.boardSetup(k) shouldEqual originalGameState.boardSetup(k)
    }
    loadedGameState.copy(boardSetup = Map.empty) shouldEqual originalGameState.copy(boardSetup = Map.empty)
  }

  it should "save and load a mid-game state with more complex data" in withTempFile { filePath =>
    // boardStateAfterMove now expects Map[String, SimplePieceType]
    val complexHistory = List(
        SavedTransition("7g7f", sampleBoardSetupMidGame - posToKey(Position(7,7)) + (posToKey(Position(7,6)) -> SimplePiece.FU)),
        SavedTransition("3c3d", sampleBoardSetupMidGame - posToKey(Position(7,7)) + (posToKey(Position(7,6)) -> SimplePiece.FU) - posToKey(Position(3,3)) + (posToKey(Position(3,4)) -> SimplePiece.FU)),
        SavedTransition("2h7h", sampleBoardSetupMidGame - posToKey(Position(7,7)) + (posToKey(Position(7,6)) -> SimplePiece.FU) - posToKey(Position(3,3)) + (posToKey(Position(3,4)) -> SimplePiece.FU) - posToKey(Position(2,8)) + (posToKey(Position(7,8)) -> SimplePiece.KA))
    )
    val originalGameState = GameState(
      boardSetup = sampleBoardSetupMidGame, // This is now Map[String, SimplePieceType]
      currentTurn = Player.GOTE,
      capturedPiecesPlayer1 = List(SimplePiece.FU, SimplePiece.FU, SimplePiece.KY),
      capturedPiecesPlayer2 = List(SimplePiece.GI),
      gameHistory = complexHistory
    )
    GameSaver.saveToFile(originalGameState, filePath.toString) should be a 'success
    val loadedGameState = GameSaver.loadFromFile(filePath.toString).get
    loadedGameState.boardSetup.keys should contain theSameElementsAs originalGameState.boardSetup.keys
    originalGameState.boardSetup.keys.foreach { k =>
        loadedGameState.boardSetup(k) shouldEqual originalGameState.boardSetup(k)
    }
    loadedGameState.gameHistory.zip(originalGameState.gameHistory).foreach { case (loadedTrans, origTrans) =>
        loadedTrans.move shouldEqual origTrans.move
        loadedTrans.boardStateAfterMove.keys should contain theSameElementsAs origTrans.boardStateAfterMove.keys
        origTrans.boardStateAfterMove.keys.foreach { k =>
            loadedTrans.boardStateAfterMove(k) shouldEqual origTrans.boardStateAfterMove(k)
        }
    }
    loadedGameState.copy(boardSetup = Map.empty, gameHistory = Nil) shouldEqual originalGameState.copy(boardSetup = Map.empty, gameHistory = Nil)
  }

  it should "save and load a game state with empty history" in withTempFile { filePath =>
    val originalGameState = GameState(
      boardSetup = sampleBoardSetup,
      currentTurn = Player.SENTE,
      capturedPiecesPlayer1 = List(SimplePiece.KI),
      capturedPiecesPlayer2 = Nil,
      gameHistory = emptyHistory
    )
    GameSaver.saveToFile(originalGameState, filePath.toString) should be a 'success
    val loadedGameState = GameSaver.loadFromFile(filePath.toString).get
    loadedGameState.boardSetup.keys should contain theSameElementsAs originalGameState.boardSetup.keys
    originalGameState.boardSetup.keys.foreach { k =>
        loadedGameState.boardSetup(k) shouldEqual originalGameState.boardSetup(k)
    }
    loadedGameState.copy(boardSetup = Map.empty) shouldEqual originalGameState.copy(boardSetup = Map.empty)
  }

  it should "save and load with captured pieces for both players" in withTempFile { filePath =>
    // King on 5,5
    val originalGameState = GameState(
      boardSetup = Map(posToKey(Position(5,5)) -> SimplePiece.OU),
      currentTurn = Player.GOTE,
      capturedPiecesPlayer1 = List(SimplePiece.HI, SimplePiece.KA, SimplePiece.FU, SimplePiece.FU),
      capturedPiecesPlayer2 = List(SimplePiece.KI, SimplePiece.GI, SimplePiece.KE, SimplePiece.KY),
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

package jp.sndyuk.shogi.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}
// PieceInfo is now part of GameState, so if GameState is imported, PieceInfo should be accessible.
// Or import jp.sndyuk.shogi.core.GameState.PieceInfo if needed, assuming it's defined in GameState object
// For SavedTransition, boardStateAfterMove now uses Map[String, PieceInfo] as per recent changes
// We need PieceInfo for the updated boardSetup types.
import jp.sndyuk.shogi.core.{PieceInfo => CorePieceInfo}
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

  // Sample data for board setup using CorePieceInfo
  val sampleBoardSetup: Map[String, CorePieceInfo] = Map(
    posToKey(Position(1, 1)) -> CorePieceInfo(SimplePiece.KY, Player.SENTE, isPromoted = false),
    posToKey(Position(1, 2)) -> CorePieceInfo(SimplePiece.KE, Player.SENTE, isPromoted = false),
    posToKey(Position(5, 5)) -> CorePieceInfo(SimplePiece.OU, Player.SENTE, isPromoted = false)
  )

  val sampleBoardSetupMidGame: Map[String, CorePieceInfo] = Map(
    posToKey(Position(7, 6)) -> CorePieceInfo(SimplePiece.FU, Player.SENTE, isPromoted = false), // Sente FU
    posToKey(Position(3, 4)) -> CorePieceInfo(SimplePiece.FU, Player.GOTE, isPromoted = false),  // Gote FU
    posToKey(Position(5, 8)) -> CorePieceInfo(SimplePiece.OU, Player.SENTE, isPromoted = false), // Sente OU
    posToKey(Position(5, 2)) -> CorePieceInfo(SimplePiece.OU, Player.GOTE, isPromoted = false),  // Gote OU
    posToKey(Position(2, 2)) -> CorePieceInfo(SimplePiece.HI, Player.GOTE, isPromoted = false),  // Gote HI
    posToKey(Position(8, 8)) -> CorePieceInfo(SimplePiece.KA, Player.SENTE, isPromoted = false)  // Sente KA
  )

  // For history, boardStateAfterMove now expects Map[String, CorePieceInfo]
  val sampleHistory: List[SavedTransition] = List(
    SavedTransition("7g7f", sampleBoardSetupMidGame - posToKey(Position(7,7)) + (posToKey(Position(7,6)) -> CorePieceInfo(SimplePiece.FU, Player.SENTE, false))),
    SavedTransition("3c3d", sampleBoardSetupMidGame - posToKey(Position(7,7)) + (posToKey(Position(7,6)) -> CorePieceInfo(SimplePiece.FU, Player.SENTE, false)) - posToKey(Position(3,3)) + (posToKey(Position(3,4)) -> CorePieceInfo(SimplePiece.FU, Player.GOTE, false)))
  )

  val emptyHistory: List[SavedTransition] = Nil

  "GameSaver" should "save and load a simple game state correctly" in withTempFile { filePath =>
    val originalGameState = GameState(
      boardSetup = sampleBoardSetup, // This is now Map[String, CorePieceInfo]
      currentTurn = Player.SENTE,
      capturedPiecesPlayer1 = List(SimplePiece.KA, SimplePiece.FU),
      capturedPiecesPlayer2 = List(SimplePiece.HI),
      gameHistory = sampleHistory,
      evaluationScore = 0
    )

    val saveResult = GameSaver.saveToFile(originalGameState, filePath.toString)
    saveResult should be a 'success

    val loadResult = GameSaver.loadFromFile(filePath.toString)
    loadResult should be a 'success

    val loadedGameState = loadResult.get
    // Custom equality check for CorePieceInfo maps
    loadedGameState.boardSetup.keys should contain theSameElementsAs originalGameState.boardSetup.keys
    originalGameState.boardSetup.keys.foreach { k =>
        loadedGameState.boardSetup(k) shouldEqual originalGameState.boardSetup(k)
    }
    // Compare other fields after emptying boardSetup for simplicity if direct GameState equality fails due to Map comparison
    loadedGameState.copy(boardSetup = Map.empty) shouldEqual originalGameState.copy(boardSetup = Map.empty)
  }

  it should "save and load an initial game state" in withTempFile { filePath =>
    val initialBoard: Map[String, CorePieceInfo] = Map(
      posToKey(Position(1,1)) -> CorePieceInfo(SimplePiece.KY, Player.SENTE, false), posToKey(Position(2,1)) -> CorePieceInfo(SimplePiece.KE, Player.SENTE, false),
      posToKey(Position(9,9)) -> CorePieceInfo(SimplePiece.KY, Player.GOTE, false), posToKey(Position(8,9)) -> CorePieceInfo(SimplePiece.KE, Player.GOTE, false)
    )
    val originalGameState = GameState(
      boardSetup = initialBoard, // This is now Map[String, CorePieceInfo]
      currentTurn = Player.SENTE,
      capturedPiecesPlayer1 = Nil,
      capturedPiecesPlayer2 = Nil,
      gameHistory = emptyHistory,
      evaluationScore = 0
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
    // boardStateAfterMove now expects Map[String, CorePieceInfo]
    val complexHistory = List(
        SavedTransition("7g7f", sampleBoardSetupMidGame - posToKey(Position(7,7)) + (posToKey(Position(7,6)) -> CorePieceInfo(SimplePiece.FU, Player.SENTE, false))),
        SavedTransition("3c3d", sampleBoardSetupMidGame - posToKey(Position(7,7)) + (posToKey(Position(7,6)) -> CorePieceInfo(SimplePiece.FU, Player.SENTE, false)) - posToKey(Position(3,3)) + (posToKey(Position(3,4)) -> CorePieceInfo(SimplePiece.FU, Player.GOTE, false))),
        SavedTransition("2h7h", sampleBoardSetupMidGame - posToKey(Position(7,7)) + (posToKey(Position(7,6)) -> CorePieceInfo(SimplePiece.FU, Player.SENTE, false)) - posToKey(Position(3,3)) + (posToKey(Position(3,4)) -> CorePieceInfo(SimplePiece.FU, Player.GOTE, false)) - posToKey(Position(2,8)) + (posToKey(Position(7,8)) -> CorePieceInfo(SimplePiece.KA, Player.SENTE, false)))
    )
    val originalGameState = GameState(
      boardSetup = sampleBoardSetupMidGame, // This is now Map[String, CorePieceInfo]
      currentTurn = Player.GOTE,
      capturedPiecesPlayer1 = List(SimplePiece.FU, SimplePiece.FU, SimplePiece.KY),
      capturedPiecesPlayer2 = List(SimplePiece.GI),
      gameHistory = complexHistory,
      evaluationScore = 0
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
            // Assuming CorePieceInfo has a sensible equals method or is a case class
            loadedTrans.boardStateAfterMove(k) shouldEqual origTrans.boardStateAfterMove(k)
        }
    }
    loadedGameState.copy(boardSetup = Map.empty, gameHistory = Nil) shouldEqual originalGameState.copy(boardSetup = Map.empty, gameHistory = Nil)
  }

  it should "save and load a game state with empty history" in withTempFile { filePath =>
    val originalGameState = GameState(
      boardSetup = sampleBoardSetup, // This is now Map[String, CorePieceInfo]
      currentTurn = Player.SENTE,
      capturedPiecesPlayer1 = List(SimplePiece.KI),
      capturedPiecesPlayer2 = Nil,
      gameHistory = emptyHistory,
      evaluationScore = 0
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
      boardSetup = Map(posToKey(Position(5,5)) -> CorePieceInfo(SimplePiece.OU, Player.SENTE, false)), // Sente King
      currentTurn = Player.GOTE,
      capturedPiecesPlayer1 = List(SimplePiece.HI, SimplePiece.KA, SimplePiece.FU, SimplePiece.FU),
      capturedPiecesPlayer2 = List(SimplePiece.KI, SimplePiece.GI, SimplePiece.KE, SimplePiece.KY),
      gameHistory = sampleHistory, // sampleHistory already uses PieceInfo in its boardStateAfterMove
      evaluationScore = 0
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

package jp.sndyuk.shogi.core

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import jp.sndyuk.shogi.core.Player.{Player => GamePlayer}
import jp.sndyuk.shogi.core.SimplePiece.{SimplePieceType => GameSimplePieceType}

class ShogiGameServiceSpec extends AnyFlatSpec with Matchers {

  "ShogiGameService" should "start a new game with default initial Shogi setup" in {
    val service = new ShogiGameService() // Calls startNewGame() internally
    val gameState = service.getGameState()

    gameState.currentTurn shouldBe Player.SENTE
    gameState.gameHistory shouldBe empty

    // Check some key initial positions
    // Sente King: Position(4,8) from core Point(8,4)
    gameState.boardSetup(Position(4,8)) shouldBe SimplePiece.OU
    // Gote King: Position(4,0) from core Point(0,4)
    gameState.boardSetup(Position(4,0)) shouldBe SimplePiece.OU
    // Sente Pawn at 7g: Position(2,6) from core Point(6,2)
    gameState.boardSetup(Position(2,6)) shouldBe SimplePiece.FU

    gameState.capturedPiecesPlayer1 shouldBe empty
    gameState.capturedPiecesPlayer2 shouldBe empty
  }

  it should "start a new game with a custom setup" in {
    val service = new ShogiGameService()
    val customBoardSetup: Map[Position, (GameSimplePieceType, GamePlayer, Boolean)] = Map(
      Position(4,8) -> ((SimplePiece.OU, Player.SENTE, false)),
      Position(0,0) -> ((SimplePiece.FU, Player.GOTE, false))
    )
    val senteCaptured = List(SimplePiece.HI)
    val goteCaptured = List(SimplePiece.KA)

    val gameState = service.startNewGame(Some(customBoardSetup), senteCaptured, goteCaptured, Player.GOTE)

    gameState.currentTurn shouldBe Player.GOTE
    gameState.boardSetup.size shouldBe 2
    gameState.boardSetup(Position(4,8)) shouldBe SimplePiece.OU
    gameState.boardSetup(Position(0,0)) shouldBe SimplePiece.FU
    
    gameState.capturedPiecesPlayer1 should contain only (SimplePiece.HI)
    gameState.capturedPiecesPlayer2 should contain only (SimplePiece.KA)
    gameState.gameHistory shouldBe empty
  }

  it should "correctly map game history in getGameState" in {
    val service = new ShogiGameService() // Standard game
    // Make a Sente move: Pawn 7g to 7f. Pos(2,6) to Pos(2,5)
    service.makeMove(Position(2,6), Position(2,5), promotion = false)
    // Make a Gote move: Pawn 3c to 3d. Pos(6,2) to Pos(6,3)
    service.makeMove(Position(6,2), Position(6,3), promotion = false)

    val gameState = service.getGameState()
    gameState.gameHistory should have size 2

    val firstMove = gameState.gameHistory.head
    firstMove.move shouldBe "7g7f" // Sente Pawn 7g to 7f
    firstMove.boardStateAfterMove(Position(2,5)) shouldBe SimplePiece.FU
    firstMove.boardStateAfterMove.get(Position(2,6)) shouldBe None

    val secondMove = gameState.gameHistory(1)
    secondMove.move shouldBe "3c3d" // Gote Pawn 3c to 3d
    secondMove.boardStateAfterMove(Position(6,3)) shouldBe SimplePiece.FU
    secondMove.boardStateAfterMove.get(Position(6,2)) shouldBe None
  }

  it should "make valid moves and update game state" in {
    val service = new ShogiGameService()

    // Sente Pawn 7g to 7f
    val move1Result = service.makeMove(Position(2,6), Position(2,5), promotion = false)
    move1Result shouldBe a [Right[_,_]]
    val gameState1 = move1Result.getOrElse(fail("Move 1 failed"))
    
    gameState1.currentTurn shouldBe Player.GOTE
    gameState1.boardSetup(Position(2,5)) shouldBe SimplePiece.FU
    gameState1.boardSetup.get(Position(2,6)) shouldBe None
    gameState1.gameHistory should have size 1
    gameState1.gameHistory.head.move shouldBe "7g7f"

    // Gote Pawn 3c to 3d
    val move2Result = service.makeMove(Position(6,2), Position(6,3), promotion = false)
    move2Result shouldBe a [Right[_,_]]
    val gameState2 = move2Result.getOrElse(fail("Move 2 failed"))

    gameState2.currentTurn shouldBe Player.SENTE
    gameState2.boardSetup(Position(6,3)) shouldBe SimplePiece.FU
    gameState2.gameHistory should have size 2
    gameState2.gameHistory(1).move shouldBe "3c3d"
  }

  it should "reject invalid moves" in {
    val service = new ShogiGameService()
    // Try to move Sente's King like a Rook
    val invalidMoveResult = service.makeMove(Position(4,8), Position(4,0), promotion = false)
    invalidMoveResult shouldBe a [Left[_,_]]
    invalidMoveResult.left.getOrElse("") should include ("Invalid move") 
  }
  
  it should "handle piece drops correctly" in {
    val service = new ShogiGameService()
    val customSetup: Map[Position, (GameSimplePieceType, GamePlayer, Boolean)] = Map(
      Position(4,8) -> ((SimplePiece.OU, Player.SENTE, false)) 
    )
    val senteCaptured = List(SimplePiece.FU)
    service.startNewGame(Some(customSetup), senteCaptured, Nil, Player.SENTE)

    val dropResult = service.makeMove(fromPos = Position(0,0), 
                                      toPos = Position(4,4), 
                                      promotion = false, 
                                      droppedPieceType = Some(SimplePiece.FU))
    
    dropResult shouldBe a [Right[_,_]]
    val gameState = dropResult.getOrElse(fail("Drop move failed"))
    gameState.boardSetup(Position(4,4)) shouldBe SimplePiece.FU
    gameState.currentTurn shouldBe Player.GOTE
    gameState.capturedPiecesPlayer1 shouldBe empty 
    gameState.gameHistory.head.move shouldBe "P*5e"
  }

  it should "handle promotion correctly" in {
    val service = new ShogiGameService()
    val customSetup: Map[Position, (GameSimplePieceType, GamePlayer, Boolean)] = Map(
      Position(6,1) -> ((SimplePiece.FU, Player.SENTE, false)), // Sente FU at 2c (core Point(1,6))
      Position(4,8) -> ((SimplePiece.OU, Player.SENTE, false)), // Sente King
      Position(4,0) -> ((SimplePiece.OU, Player.GOTE, false))  // Gote King
    )
    service.startNewGame(initialBoardSetup = Some(customSetup), firstPlayer = Player.SENTE)

    // Verify piece setup before making the move
    val pieceAtSourceBeforeMove = service.board.piece(GameStateMapper.positionToCorePoint(Position(6,1)), PlayerA)
    pieceAtSourceBeforeMove shouldBe Piece.▲.FU // Expect Sente FU at core Point(1,6)

    // Sente FU 2c (Position(6,1)) to 2b (Position(1,1)), promote.
    val promoteResult = service.makeMove(Position(6,1), Position(1,1), promotion = true) // Corrected fromPos
    promoteResult shouldBe a [Right[_,_]]
    val gameState = promoteResult.getOrElse(fail("Promotion move failed"))
    
    gameState.gameHistory.head.move shouldBe "2c2b+"
    val coreBoard = service.board 
    val pieceOnBoard = coreBoard.squares.get(GameStateMapper.positionToCorePoint(Position(1,1)))
    Piece.isPromoted(pieceOnBoard) shouldBe true
    Piece.generalize(pieceOnBoard) shouldBe Piece.◯.FU
  }

  it should "get valid moves for a pawn" in {
    val service = new ShogiGameService() 
    val validMoves = service.getValidMoves(Position(2,6))
    validMoves should contain only (Position(2,5)) 
  }

  it should "get valid moves for a rook" in {
    val service = new ShogiGameService() 
    val validMoves = service.getValidMoves(Position(7,7))
    
    val expectedMoves = List(
      Position(6,7), Position(5,7), Position(4,7), Position(3,7), Position(2,7), // Moves left
      Position(8,7)  // Move right
    )
    validMoves should contain allElementsOf expectedMoves
    validMoves.size shouldBe expectedMoves.size 

    validMoves should not contain Position(1,7) 
    validMoves should not contain Position(0,7) 
    validMoves should not contain Position(7,6) 
    validMoves should not contain Position(7,8) 
  }
}

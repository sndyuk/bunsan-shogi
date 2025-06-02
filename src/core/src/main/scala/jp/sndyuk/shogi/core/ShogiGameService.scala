package jp.sndyuk.shogi.core

import jp.sndyuk.shogi.core.Player.Player
import jp.sndyuk.shogi.core.SimplePiece.SimplePieceType

class ShogiGameService {

  var board: Board = _
  var currentState: State = _
  // Store the initial setup parameters to aid history replay
  private var initialGameFirstPlayer: Player = Player.SENTE
  private var initialGameSetup: Option[Map[Position, (SimplePieceType, Player, Boolean)]] = None
  private var initialGameSenteCaptured: List[SimplePieceType] = Nil
  private var initialGameGoteCaptured: List[SimplePieceType] = Nil


  // Initialize a new game upon service creation using default parameters
  startNewGame()

  def startNewGame(
    initialBoardSetup: Option[Map[Position, (SimplePieceType, Player, Boolean)]] = None,
    initialSenteCaptured: List[SimplePieceType] = Nil,
    initialGoteCaptured: List[SimplePieceType] = Nil,
    firstPlayer: Player = Player.SENTE
  ): GameState = {
    // Store initial parameters for potential future use (e.g. robust history replay)
    this.initialGameFirstPlayer = firstPlayer
    this.initialGameSetup = initialBoardSetup
    this.initialGameSenteCaptured = initialSenteCaptured
    this.initialGameGoteCaptured = initialGoteCaptured

    this.board = initialBoardSetup match {
      case Some(setup) =>
        GameStateMapper.reconstructCoreBoard(setup, initialSenteCaptured, initialGoteCaptured)
      case None =>
        val standardBoard = Board() // Initializes to standard shogi setup
        // If initialSenteCaptured or initialGoteCaptured are provided without a custom board setup,
        // they are currently ignored. If they should apply to a standard board, this needs adjustment.
        // For now, assuming they only make sense with a full custom initialBoardSetup.
        if (initialSenteCaptured.nonEmpty || initialGoteCaptured.nonEmpty) {
            // This case might need clarification: custom captured pieces with standard board setup.
            // For now, reconstructCoreBoard expects a boardSetup if captured pieces are specified.
            // A simple way is to provide the standard board setup to reconstructCoreBoard.
            // However, reconstructCoreBoard creates an empty board first.
            // So, if we want standard board + custom captured, we'd need to:
            // 1. Get standard board.
            // 2. Add custom captured pieces to it.
            // This logic is not currently in reconstructCoreBoard.
            // For simplicity, current behavior: standard board means no custom captured pieces from params.
        }
        standardBoard
    }
    this.currentState = State(Nil, GameStateMapper.playerToCoreTurn(firstPlayer))
    getGameState()
  }

  private def initialBoardForHistoryReplay(): Board = {
    // Use the stored initial parameters to reconstruct the board as it was at the start of this game instance
    this.initialGameSetup match {
      case Some(setup) =>
        GameStateMapper.reconstructCoreBoard(setup, this.initialGameSenteCaptured, this.initialGameGoteCaptured)
      case None =>
        // If no custom setup, it was a standard Board(). Captured pieces should be empty then by current startNewGame logic.
        Board()
    }
  }

  def getGameState(): GameState = {
    val boardSetup = GameStateMapper.coreBoardToBoardSetup(this.board)
    val currentTurnPlayer = GameStateMapper.coreTurnToPlayer(this.currentState.turn)

    val senteCapturedPieces: List[SimplePieceType] = Piece.◯.all.flatMap { generalizedPiece =>
      val count = this.board.capturedPieces.count(PlayerA, generalizedPiece)
      List.fill(count)(
        GameStateMapper.corePieceToSimplePieceTypeAndPlayer(generalizedPiece) match {
          case Some((spt, _, _)) => spt
          case None => throw new IllegalStateException(s"Could not map generalized captured piece $generalizedPiece to SimplePieceType")
        }
      )
    }.toList

    val goteCapturedPieces: List[SimplePieceType] = Piece.◯.all.flatMap { generalizedPiece =>
      val count = this.board.capturedPieces.count(PlayerB, generalizedPiece)
      List.fill(count)(
        GameStateMapper.corePieceToSimplePieceTypeAndPlayer(generalizedPiece) match {
          case Some((spt, _, _)) => spt
          case None => throw new IllegalStateException(s"Could not map generalized captured piece $generalizedPiece to SimplePieceType")
        }
      )
    }.toList
    
    val gameHistoryMapped: List[SimpleTransition] = {
      if (this.currentState.history.isEmpty) {
        Nil
      } else {
        val gameStartingTurnFromService = GameStateMapper.playerToCoreTurn(this.initialGameFirstPlayer)
        
        // Initial accumulator: (board state for the start of history, empty list of SimpleTransitions)
        val initialAccumulator = (initialBoardForHistoryReplay(), List.empty[SimpleTransition])

        val (_, transitionsReversed) = 
          this.currentState.history.reverse.zipWithIndex.foldLeft(initialAccumulator) { 
            case ((currentBoardState, accumulatedTransitions), (coreTrans, index)) =>
              
              val boardBeforeThisMove = currentBoardState.copy() // Copy for "before" state
              val playerForThisTransition = if (index % 2 == 0) gameStartingTurnFromService else gameStartingTurnFromService.change
              
              val dummyStateForHistoryMove = State(Nil, playerForThisTransition)
              // This move mutates currentBoardState (the one inside the accumulator)
              currentBoardState.move(dummyStateForHistoryMove, coreTrans.oldPos, coreTrans.newPos, validation = false, nari = coreTrans.nari)
              
              val boardAfterThisMove = currentBoardState.copy() // Copy for "after" state (after mutation)
              
              val simpleTrans = GameStateMapper.coreTransitionToSimpleTransition(coreTrans, boardBeforeThisMove, boardAfterThisMove)
              
              (currentBoardState, simpleTrans :: accumulatedTransitions) // Pass mutated board state and new transition
          }
        transitionsReversed.reverse // Reverse to get chronological order
      }
    }

    GameState(
      boardSetup = boardSetup,
      currentTurn = currentTurnPlayer,
      capturedPiecesPlayer1 = senteCapturedPieces,
      capturedPiecesPlayer2 = goteCapturedPieces,
      gameHistory = gameHistoryMapped
    )
  }

  def makeMove(
    fromPos: Position,
    toPos: Position,
    promotion: Boolean,
    droppedPieceType: Option[SimplePieceType] = None
  ): Either[String, GameState] = {

    val newCorePoint = GameStateMapper.positionToCorePoint(toPos)
    var pieceToMove: Piece = Piece.❏
    val oldCorePoint: Point = droppedPieceType match {
      case Some(spt) =>
        val turn = this.currentState.turn
        val corePieceDropped = GameStateMapper.simplePiecePlayerToCorePiece(spt, GameStateMapper.coreTurnToPlayer(turn), false)
        pieceToMove = corePieceDropped
        Point.ofCaptured(Piece.generalize(corePieceDropped))
      case None =>
        val op = GameStateMapper.positionToCorePoint(fromPos)
        // Use board.piece to correctly fetch from board OR captured set if op indicates a captured piece
        pieceToMove = this.board.piece(op, this.currentState.turn) 
        if (pieceToMove == Piece.❏) { // Check if the determined piece is empty
             return Left(s"Invalid move: No piece at source position $fromPos (x=${fromPos.x}, y=${fromPos.y}; core op: x=${op.x}, y=${op.y}) or specified captured piece not available.")
        }
        op
    }
    
    if (pieceToMove == Piece.❏) { // Should be caught by specific drop/move logic, but as a safeguard
        return Left("Invalid move: Selected piece is empty or could not be determined.")
    }

    if (Rule.canMove(this.board, pieceToMove, oldCorePoint, newCorePoint, this.currentState.turn, promotion)) {
      // The board.move method will mutate `this.board` and return a new State with updated history and turn.
      val nextState = this.board.move(this.currentState, oldCorePoint, newCorePoint, validation = false, nari = promotion)
      this.currentState = nextState 
      Right(getGameState())
    } else {
      Left("Invalid move: Rule violation.")
    }
  }

  def getValidMoves(fromPosValue: Position): List[Position] = {
    val coreFromPoint = GameStateMapper.positionToCorePoint(fromPosValue)
    // Correctly get the piece from board or hand.
    // board.piece(point, turn) handles if point is a captured piece point or board point.
    val piece = this.board.piece(coreFromPoint, this.currentState.turn)

    if (piece == Piece.❏) {
      // If coreFromPoint was a board point, it means empty square.
      // If coreFromPoint was a captured piece point, board.piece would return ❏ if that piece isn't in hand.
      return Nil 
    }

    // Rule.generateMovablePoints takes oldPos (which can be a captured piece point)
    // includePromoted = true to see all promotion possibilities
    Rule.generateMovablePoints(this.board, coreFromPoint, piece, this.currentState.turn, includePromoted = true)
      .map { case (targetPoint, _) => GameStateMapper.corePointToPosition(targetPoint) } // We only need the target position
      .toList
      .distinct // Moves might result in same newPos (e.g. with and without promotion if piece can't promote there)
                // but Position doesn't carry promotion info.
  }
}

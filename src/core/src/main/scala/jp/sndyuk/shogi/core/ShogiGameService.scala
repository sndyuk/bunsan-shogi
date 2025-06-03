package jp.sndyuk.shogi.core

import jp.sndyuk.shogi.core.Player.Player
import jp.sndyuk.shogi.core.SimplePiece.SimplePieceType
import jp.sndyuk.shogi.ai.{ShogiAI, AlphaBetaAI_V1, AlphaBetaAI_V2}
// Removed: import jp.sndyuk.shogi.core.Transition // This was causing "permanently hidden" error

object AIProvider {
  def getAI(aiType: String, searchDepth: Int): Option[ShogiAI] = {
    aiType.toLowerCase match {
      case "v1" => Some(new AlphaBetaAI_V1("AlphaBetaAI_V1", searchDepth))
      case "v2" => Some(new AlphaBetaAI_V2("AlphaBetaAI_V2", searchDepth))
      case _    => None
    }
  }
}

// NOTE: The `suggestMove` method has been moved into ShogiGameService class.
// This comment block is a placeholder for the removed incorrect placement.

class ShogiGameService {

  var board: Board = _
  var currentState: State = _
  var aiOpponent: Option[ShogiAI] = None
  var gameMode: String = "hvh" // "hvh", "hva_sente", "hva_gote"
  var aiSearchDepth: Int = 3
  // Store the initial setup parameters to aid history replay
  private var initialGameFirstPlayer: Player = Player.SENTE
  private var initialGameSetup: Option[Map[Position, (SimplePieceType, Player, Boolean)]] = None
  private var initialGameSenteCaptured: List[SimplePieceType] = Nil
  private var initialGameGoteCaptured: List[SimplePieceType] = Nil


  // Initialize a new game upon service creation using default parameters
  startNewGame(gameMode = "hvh", aiType = "v2", aiSearchDepth = 3)

  def startNewGame(
    initialBoardSetup: Option[Map[Position, (SimplePieceType, Player, Boolean)]] = None,
    initialSenteCaptured: List[SimplePieceType] = Nil,
    initialGoteCaptured: List[SimplePieceType] = Nil,
    firstPlayer: Player = Player.SENTE,
    gameMode: String = "hvh",
    aiType: String = "v2",
    aiSearchDepth: Int = 3
  ): GameState = {
    // Store initial parameters for potential future use (e.g. robust history replay)
    this.initialGameFirstPlayer = firstPlayer
    this.initialGameSetup = initialBoardSetup
    this.initialGameSenteCaptured = initialSenteCaptured
    this.initialGameGoteCaptured = initialGoteCaptured

    this.gameMode = gameMode
    this.aiSearchDepth = aiSearchDepth

    if (gameMode == "hva_sente" || gameMode == "hva_gote") {
      this.aiOpponent = AIProvider.getAI(aiType, this.aiSearchDepth)
      if (this.aiOpponent.isEmpty) {
        println(s"Warning: Could not initialize AI with type '$aiType'. Game will be Human vs Human.")
        // For now, it will default to no AI opponent.
      }
    } else {
      this.aiOpponent = None
    }

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
          case Some((spt, _, _)) => spt // spt should be SimplePieceType
          case None => throw new IllegalStateException(s"Could not map generalized captured piece $generalizedPiece to SimplePieceType")
        }
      )
    }.toList

    // gameHistory in GameState is List[SimpleTransition]
    // currentState.history is List[CoreTransition]
    val gameHistoryMapped: List[jp.sndyuk.shogi.core.SimpleTransition] = { // FQN for SimpleTransition List
      if (this.currentState.history.isEmpty) {
        Nil
      } else {
        val gameStartingTurnFromService = GameStateMapper.playerToCoreTurn(this.initialGameFirstPlayer)
        val initialBoardForReplay = initialBoardForHistoryReplay()

        // Fold over the core history (List[jp.sndyuk.shogi.core.Transition])
        val (_, simpleTransitionsReversed) =
          this.currentState.history.foldLeft((initialBoardForReplay, List.empty[jp.sndyuk.shogi.core.SimpleTransition])) { // FQN for SimpleTransition List
            // coreTrans is item from currentState.history, which is List[jp.sndyuk.shogi.core.Transition]
            case ((currentBoardSim, accTransitions), coreTrans: jp.sndyuk.shogi.core.Transition) => // FQN for coreTrans type
              val boardBeforeThisMove = currentBoardSim.copy()
              val playerForThisTransition = if (accTransitions.size % 2 == 0) gameStartingTurnFromService else gameStartingTurnFromService.change
              val dummyStateForHistoryMove = State(Nil, playerForThisTransition) // State is jp.sndyuk.shogi.core.State
              currentBoardSim.move(dummyStateForHistoryMove, coreTrans.oldPos, coreTrans.newPos, validation = false, nari = coreTrans.nari)
              val boardAfterThisMove = currentBoardSim.copy()

              val simpleTrans = GameStateMapper.coreTransitionToSimpleTransition(coreTrans, boardBeforeThisMove, boardAfterThisMove)
              (currentBoardSim, simpleTrans :: accTransitions)
          }
        simpleTransitionsReversed // Already chronological due to foldLeft on history and prepending to accumulator
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

  def requestAIMove(): Either[String, GameState] = {
    val currentPlayer = GameStateMapper.coreTurnToPlayer(this.currentState.turn)
    val isAISenteTurn = gameMode == "hva_sente" && currentPlayer == Player.SENTE
    val isAIGoteTurn = gameMode == "hva_gote" && currentPlayer == Player.GOTE

    if (aiOpponent.isDefined && (isAISenteTurn || isAIGoteTurn)) {
      aiOpponent.get.findBestMove(this.currentState, this.board, this.currentState.turn, this.aiSearchDepth) match {
        case (Some(transition: jp.sndyuk.shogi.core.Transition), nodesVisited) => // FQN for Transition
          // println(s"AI (${if (isAISenteTurn) "SENTE" else "GOTE"}) found move: $transition, Nodes visited: $nodesVisited")

          val coreFromPos = transition.oldPos
          val coreToPos = transition.newPos
          val promotion = transition.nari

          var droppedPieceType: Option[SimplePieceType] = None // Renamed for clarity
          val isDrop = Point.isCaptured(coreFromPos)

          if (isDrop) {
            // When dropping, coreFromPos represents the piece type in hand.
            // this.board.piece(coreFromPos, turn) correctly gets the Piece enum (e.g. Piece.sFU)
            val pieceEnumToDrop = this.board.piece(coreFromPos, this.currentState.turn)
            if (pieceEnumToDrop != Piece.❏) { // Check if the piece is actually in hand
              GameStateMapper.corePieceToSimplePieceTypeAndPlayer(pieceEnumToDrop) match {
                case Some((spt, _, _)) => droppedPieceType = Some(spt)
                case None =>
                  // This should ideally not happen if GameStateMapper is comprehensive
                  return Left(s"Error: AI tried to drop an unmappable piece '$pieceEnumToDrop' (could not map to SimplePieceType).")
              }
            } else {
              // This case means AI chose a piece to drop that isn't in its hand according to board.piece
              // coreFromPos.x directly holds the Int value of the generalized piece intended for drop.
              val intendedGeneralizedPiece: Piece = coreFromPos.x
              return Left(s"Error: AI selected an invalid hand piece for drop (Piece: ${Piece.name(intendedGeneralizedPiece)}), or piece not available.")
            }
          }

          val fromPosMapped = GameStateMapper.corePointToPosition(coreFromPos) // Will map to Position(0,0) for drops if old mapping is kept
          val toPosMapped = GameStateMapper.corePointToPosition(coreToPos)

          // Call makeMove with the inferred drop information
          makeMove(fromPosMapped, toPosMapped, promotion, droppedPieceType)

        case (None, nodesVisited) =>
          // println(s"AI (${if (isAISenteTurn) "SENTE" else "GOTE"}) found no move. Nodes visited: $nodesVisited")
          Left("AI found no valid move or game has ended.")
      }
    } else {
      Left("Not AI's turn or no AI opponent configured.")
    }
  }

  def suggestMove(aiType: String, searchDepth: Int): Either[String, jp.sndyuk.shogi.core.SimpleTransition] = { // FQN for SimpleTransition
    AIProvider.getAI(aiType = aiType, searchDepth = searchDepth) match {
      case None => Left(s"Unknown AI type: $aiType")
      case Some(ai) =>
        val currentBoardCopy = this.board.copy()
        val currentStateCopy = this.currentState.copy()
        val turnForAI = this.currentState.turn

        val (optTransition, nodesVisited) = ai.findBestMove(currentStateCopy, currentBoardCopy, turnForAI, searchDepth)

        optTransition match {
          case Some(coreTrans: jp.sndyuk.shogi.core.Transition) => // FQN for Transition
            val tempBoardAfterMove = currentBoardCopy.copy()
            tempBoardAfterMove.move(currentStateCopy, coreTrans.oldPos, coreTrans.newPos, validation = false, nari = coreTrans.nari)
            val simpleTrans = GameStateMapper.coreTransitionToSimpleTransition(coreTrans, currentBoardCopy, tempBoardAfterMove)
            Right(simpleTrans)
          case None =>
            Left("AI could not suggest a valid move (game might be at an end state or AI error).")
        }
    }
  }
}

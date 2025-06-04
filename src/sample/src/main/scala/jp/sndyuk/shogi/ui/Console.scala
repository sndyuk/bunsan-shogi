package jp.sndyuk.shogi.ui

// This object provides a console-based interface to play a game of Shogi,
// typically between a human player and an AI, or human vs human.
// It uses the Shogi trait to manage game flow.

import jp.sndyuk.shogi.core.Point
import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.player.HumanPlayer
import jp.sndyuk.shogi.player.AIPlayer
import jp.sndyuk.shogi.player.Player
import jp.sndyuk.shogi.core.State
import jp.sndyuk.shogi.core.Transition
import jp.sndyuk.shogi.player.CommandReader
import jp.sndyuk.shogi.core.PlayerB // Gote (second player)
// import jp.sndyuk.shogi.core.PlayerA // Sente (first player) - Unused import
import jp.sndyuk.shogi.ai.{AlphaBetaAI_V1} // Available AI implementations - AlphaBetaAI_V2 was unused

object Console extends App with Shogi {

  override val board = Board() // Initializes the Shogi board.

  // commandReader is responsible for parsing human player input from the console.
  val commandReader = new CommandReader {

    // The routeRegex defines the expected format for human input to make a move.
    // Format: (筋,段)(筋,段)[+]
    //   - (筋,段): Represents coordinates on the Shogi board.
    //     - 筋 (suji): File/column, numbered 1 (rightmost) to 9 (leftmost) from Player A's (Sente) perspective.
    //     - 段 (dan): Rank/row, numbered 1 (topmost) to 9 (bottommost) from Player A's (Sente) perspective.
    //   - The first (筋,段) is the starting position of the piece.
    //   - The second (筋,段) is the destination position.
    //   - [+]: Optional character indicating promotion if the move allows it.
    // Example: (7,7)(7,6) moves a piece from 7g to 7f.
    // Example: (2,2)(2,1)+ moves a piece from 2b to 2a and promotes it.
    private[this] val routeRegex = """\((\d+),(\d+)\)\((\d+),(\d+)\)([\+])?""".r

    def read(state: State): Transition = {
      val inputLine = scala.Console.in.readLine
      routeRegex.findFirstMatchIn(inputLine) match {
        case Some(m) =>
          val a = m.group(1)
          val b = m.group(2)
          val c = m.group(3)
          val d = m.group(4)
          val e = m.group(5) // This can be null if the '+' is not present

          // Board.humanReadableToPoint converts the 1-indexed (file, rank) from human input
          // to the 0-indexed internal representation used by the Board.
          val oldPos = Board.humanReadableToPoint(a.toInt, b.toInt)
          val newPos = Board.humanReadableToPoint(c.toInt, d.toInt)
          Transition(oldPos, newPos, e != null && e == "+", board.pieceOnBoardNotEmpty(newPos))
        case None =>
          throw new IllegalArgumentException("Invalid command format. Please use (file,rank)(file,rank)[+].")
      }
    }
  }

  // Player Configuration:
  // The game is set up with Player A (Sente) as a Human and Player B (Gote) as an AI.
  // You can change this configuration by modifying the playerA and playerB definitions.

  // Player A (Sente - typically starts) is a Human player.
  override val playerA: Player = new HumanPlayer("Human_PlayerA (Sente)", board, commandReader, true)
  // To make Player A an AI Player:
  // override val playerA: Player = new AIPlayer("AI_PlayerA (Sente)", PlayerA, new AlphaBetaAI_V2(name = "AI_A", searchDepth = 2), 2)
  // override val playerA: Player = new AIPlayer("AI_PlayerA (Sente)", PlayerA, new AlphaBetaAI_V1(name = "AI_A", searchDepth = 1), 1)


  // Player B (Gote) is an AI player using AlphaBetaAI_V1 with search depth 1.
  override val playerB: Player = new AIPlayer("AI_PlayerB (Gote)", PlayerB, new AlphaBetaAI_V1(name = "AI_B", searchDepth = 1), 1)
  // To make Player B a Human Player:
  // override val playerB: Player = new HumanPlayer("Human_PlayerB (Gote)", board, commandReader, true)
  // To use a different AI or search depth for Player B:
  // override val playerB: Player = new AIPlayer("AI_PlayerB (Gote)", PlayerB, new AlphaBetaAI_V2(name = "AI_B", searchDepth = 2), 2)

  println(board.toString) // Display the initial board state.
  // Enhanced prompt for human input, explaining the coordinate system.
  println("\nEnter your move in the format: (File,Rank)(File,Rank)[+]")
  println("  - File (筋 suji): 1 (right) to 9 (left)")
  println("  - Rank (段 dan): 1 (top) to 9 (bottom)")
  println("  - Example: (7,7)(7,6) to move from 7g to 7f")
  println("  - Example: (2,2)(2,1)+ to move from 2b to 2a and promote")
  println("\nTo drop a piece, use its current 'in-hand' position as the source.")
  println("  This is usually represented as (PieceType,0) or similar by the system, but you need to know the piece's internal ID.")
  println("  Consult piece IDs for drops: 玉(King)=0, 歩(Pawn)=1, 金(Gold)=2, 銀(Silver)=3, 飛(Rook)=4, 角(Bishop)=5, 桂(Knight)=6, 香(Lance)=7")
  println("  Example for dropping a pawn (ID 1) from hand to 5e (5,5): (1,0)(5,5)")


  // --- Shogi Trait Override Methods ---
  // These methods are callbacks that are invoked at different stages of the game.

  /** Called after a move has been successfully made and the board has been updated. */
  override def afterMove(player: Player, oldPos: Point, newPos: Point): Unit = {
    println(board.toString) // Display the board after the move.
  }

  /** Called before it's a player's turn to move. */
  override def beforeMove(player: Player): Unit = {
    println(s"\n--- ${player}'s Turn ---") // Announce whose turn it is.
  }

  /** Called when the game has finished (e.g., checkmate, stalemate). */
  override def done(player: Player, oldPos: Point, newPos: Point, winner: Player): Unit = {
    println(s"\n--- Game Over ---")
    println(s"Move: ${player} from ${oldPos} to ${newPos}")
    println(s"Winner: ${winner}")
  }

  /** Called if a player attempts an illegal or invalid move. */
  override def failToMove(player: Player, oldPos: Point, newPos: Point): Unit = {
    println(s"Invalid move attempted by ${player}: ${oldPos} -> ${newPos}. Please try again.")
  }

  /** Called when an error occurs during input parsing or game processing. */
  override def onError(e: Exception): Unit = {
    e match {
      case _: java.util.NoSuchElementException | _: java.io.EOFException =>
        println("\nInput stream ended unexpectedly (e.g., Ctrl+D). This usually means no more input can be read.")
        println("This can happen if the program is expecting input but the source is closed.")
        println("Exiting game.")
        sys.exit(1)
      case _ =>
        println(s"\nAn error occurred: ${e.getMessage}")
        println("Could not parse your command or an internal error occurred.")
        println("Retry with a valid move? (Y/N) or show stack trace and exit (X)")
        scala.Console.in.readLine().trim().toUpperCase() match {
          case "N" =>
            println("Exiting game.")
            sys.exit
          case "X" =>
            e.printStackTrace()
            println("Exiting game due to error.")
            sys.exit
          case _ => // Default to retry
            println("Please try your move again.")
        }
    }
  }

  start() // Starts the game loop defined in the Shogi trait.
}
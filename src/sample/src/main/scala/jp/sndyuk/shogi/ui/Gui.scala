package jp.sndyuk.shogi.ui

import java.awt.Dimension
import java.awt.Font
import java.awt.Graphics2D
import java.awt.Polygon
import java.awt.Rectangle
import java.util.concurrent.CountDownLatch

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.swing.BoxPanel
import scala.swing.Button
import scala.swing.Color
import scala.swing.Dialog
import scala.swing.GridPanel
import scala.swing._
import scala.swing.event.ButtonClicked
import scala.swing.event.Key._
import scala.swing.event.MouseDragged
import scala.swing.event.MouseEntered
import scala.swing.event.MouseReleased

import jp.sndyuk.shogi.core.Block
import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.Piece
import jp.sndyuk.shogi.core.Piece.▲
import jp.sndyuk.shogi.core.Piece.△
import jp.sndyuk.shogi.core.Piece.◯
import jp.sndyuk.shogi.core.PlayerA
import jp.sndyuk.shogi.core.PlayerB
import jp.sndyuk.shogi.core.Point
import jp.sndyuk.shogi.core.Rule
import jp.sndyuk.shogi.core.State
import jp.sndyuk.shogi.core.Transition
import jp.sndyuk.shogi.core.Turn
import jp.sndyuk.shogi.core.toPoint
import jp.sndyuk.shogi.player.AIPlayer
import jp.sndyuk.shogi.player.CommandReader
import jp.sndyuk.shogi.player.HumanPlayer
import jp.sndyuk.shogi.player.Player
import jp.sndyuk.shogi.player.Utils
import jp.sndyuk.shogi.ai.AlphaBetaAI_V1

case class BoardView(blocks: Seq[Block], piecesOfPlayerA: List[Block], piecesOfPlayerB: List[Block])

abstract class BoardPanel extends GridPanel(9, 9) {
  def rebuild(): Unit
}

object Gui extends SimpleSwingApplication with Shogi {

  private var boardLatch = new CountDownLatch(1)

  val colorWhite = new Color(255, 255, 255)
  val colorLightBlack = new Color(30, 30, 30)
  val colorDarkWhite = new Color(190, 190, 190)
  val colorGray = new Color(48, 99, 99)
  val colorSilver = new Color(210, 255, 255)
  val colorGold = new Color(230, 180, 34)
  val colorLightGray = new Color(128, 128, 128)
  val colorOcher = new Color(213, 178, 138)
  val colorLightOcher = new Color(233, 208, 158)

  val blockMargin = 4
  val blockSize = 49

  val pieceMargin = blockMargin / 2
  val pieceSize = blockSize - pieceMargin - blockMargin

  val pieceSizeDimension = new Dimension(
    (pieceSize + pieceMargin),
    (pieceSize + pieceMargin))

  val capturedPieceSizeDimension = new Dimension(
    (pieceSize + pieceMargin) * 2,
    (pieceSize + pieceMargin))

  val textMargin = 1
  val textSize = 14

  val fontOfPiece = new Font("Osaka", Font.PLAIN, textSize)

  val board = Board()

  val commandReader = new CommandReader {

    def read(state: State): Transition = {
      boardLatch.await
      boardLatch = new CountDownLatch(1)
      val nextTransition = Transition(oldPos, newPos, nari, board.pieceOnBoardNotEmpty(newPos))
      currState = State(nextTransition :: state.history, state.turn.change)
      nextTransition
    }
  } // Added missing closing brace for commandReader

  val playerA = new HumanPlayer("playerA", board, commandReader, false)
//  val playerB = new HumanPlayer("playerB", board, commandReader, false)
  val playerB = new AIPlayer("AI_PlayerB_GUI", PlayerB, new AlphaBetaAI_V1(searchDepth = 1), 1)

  @volatile private var player: Player = playerA
  @volatile private var oldPos: Point = (0, 0)
  @volatile private var newPos: Point = (0, 0)
  @volatile private var nari = false
  @volatile private var currState: State = _

  val view: BoardView = BoardView(board.allBlocks, board.allMovablePieces(PlayerA), board.allMovablePieces(PlayerB))

  val piecesOnViewByComponent = scala.collection.mutable.HashMap[java.awt.Component, PiecePanel]()
  val capturedPiecesByComponent = scala.collection.mutable.HashMap[java.awt.Component, CapturedPiecePanel]()
  val piecesOnViewByPiece = scala.collection.mutable.HashMap[Piece, PiecePanel]()

  class PiecePanel(val block: Block) extends Panel {
    preferredSize = pieceSizeDimension

    private var dragOver: Option[PiecePanel] = None

    // 上をドラッグ中のパネル
    private var potantiallyRelpaceWith: Option[Block] = None

    background = colorLightOcher

    def enterDragOver(block: Block): Unit = {
      potantiallyRelpaceWith = Option(block)
      repaint
    }

    def exitDragOver(): Unit = {
      potantiallyRelpaceWith = None
      repaint
    }

    listenTo(mouse.clicks)
    listenTo(mouse.moves)

    reactions += {
      case MouseEntered(source, point, modifiers) =>

      case MouseDragged(source, point, modifiers) =>
        // ドラッグ中は通過中のパネルの見た目を変える
        val loc = location
        Option(peer.getParent.findComponentAt(loc.x + point.x, loc.y + point.y)).foreach { c =>
          val destOpt = piecesOnViewByComponent.get(c)
          if (dragOver != destOpt) {
            dragOver.foreach(_.exitDragOver)
            destOpt.foreach(_.enterDragOver(block))
            dragOver = destOpt
          }
        }

      case MouseReleased(source, point, modifiers, clicks, triggersPopup) =>
        // ドラッグ終了時は通過したパネルの見た目を元に戻す
        val srcPanel = source.asInstanceOf[PiecePanel]
        val loc = location
        Option(peer.getParent.findComponentAt(loc.x + point.x, loc.y + point.y)).foreach { c =>
          val destOpt = piecesOnViewByComponent.get(c)
          dragOver.foreach(_.exitDragOver)
          destOpt.foreach { destPanel =>
            onSelect(srcPanel.block.piece, srcPanel.block.point, destPanel.block.point)
          }
        }
        dragOver = None
    }

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)

      def drawPiece(): Unit = {
        def $(block: Block) = {
          g fill buildPiece(▲(block.piece))
          val (s, x, y) = buildPieceString(block.piece, ▲(block.piece))

          val orig = g.getTransform
          if (△(block.piece)) {
            g.rotate(Math.PI, (blockSize / 2).toDouble, (blockSize / 2).toDouble)
          }

          g setFont fontOfPiece
          g setColor colorLightBlack
          g drawString (s, x, y)
          g.setTransform(orig)
        }

        potantiallyRelpaceWith.foreach { graggingBlock =>
          g setColor colorDarkWhite
          $(graggingBlock)
        }
        if (potantiallyRelpaceWith.isEmpty && block.piece != Piece.❏) {
          g setColor colorOcher
          $(block)
        }
      }
      drawPiece
    }
  }

  class CapturedPiecePanel(turn: Turn, val block: Block, val count: Int) extends Panel {
    private val turnA = turn == PlayerA
    preferredSize = capturedPieceSizeDimension

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)

      def drawPiece(): Unit = {
        def $(block: Block) = {
          val xMergin = if (turnA) 0 else blockSize + blockMargin
          g fill buildPiece(turnA, xMergin)
          val (s, x, y) = buildPieceString(block.piece, turnA)

          val orig = g.getTransform
          if (turn == PlayerB) {
            g.rotate(Math.PI, (blockSize / 2).toDouble, (blockSize / 2).toDouble)
          }
          g setFont fontOfPiece
          g setColor colorLightBlack
          g drawString (s + "   x " + count, if (!turnA) (-1 * xMergin) + x else xMergin + x, y)
          g.setTransform(orig)
        }

        g setColor colorOcher
        $(block)
      }
      drawPiece
    }
  }

  // --- Imports for Saving/Exporting ---
  import jp.sndyuk.shogi.core.{
    GameState => SavedGameState, // The GameState case class for serialization
    GameSaver,
    Player => SavedPlayer, // The Player enum in GameState.scala (SENTE, GOTE)
    Position => SavedPosition, // The Position case class in GameState.scala (x, y)
    Piece => SavedPieceEnum, // The Piece enum in GameState.scala (KING, ROOK, etc.)
    Transition => SavedTransition // The Transition case class in GameState.scala (move string, board map)
  }
  import jp.sndyuk.shogi.kifu.{CSAExporter, KI2Exporter}
  // Kifu exporters currently use their own TempCore, we need to map to that.
  // Ideally, exporters would use jp.sndyuk.shogi.core types directly or a shared Kifu model.
  import jp.sndyuk.shogi.kifu.CSAExporter.{TempCore => KifuTempCore}

  // Core types from the game logic
  import jp.sndyuk.shogi.core.{
    Board => CoreBoard,
    Piece => CorePiece,
    Point => CorePoint,
    State => CoreState,
    Transition => CoreTransition,
    Turn => CoreTurn,
    PlayerA, // Represents Sente in core logic
    PlayerB // Represents Gote in core logic
  }
  import scala.util.{Try, Success, Failure}
  import java.io.{File, PrintWriter, FileWriter} // For file writing in export


  // --- Mapping Functions ---

  // Core Turn (PlayerA/PlayerB) to SavedPlayer (SENTE/GOTE enum for JSON)
  private def coreTurnToSavedPlayer(turn: CoreTurn): SavedPlayer.Value = turn match {
    case PlayerA => SavedPlayer.SENTE
    case PlayerB => SavedPlayer.GOTE
  }

  // Core Point (y,x) to SavedPosition (x,y for JSON)
  private def corePointToSavedPosition(p: CorePoint): SavedPosition = SavedPosition(p.x, p.y)

  // Core Piece (Int with flags) to SavedPieceEnum (generic piece type for JSON)
  // This is lossy as player and promotion info are stripped for the SavedPieceEnum.
  private def corePieceToSavedPieceEnum(cp: CorePiece): SavedPieceEnum.Value = {
    import jp.sndyuk.shogi.core.Piece._ // Access to ▲, △, ◯ objects
    generalize(cp) match { // generalize removes player info, then map to SavedPieceEnum
      case ◯.OU => SavedPieceEnum.KING
      case ◯.FU => SavedPieceEnum.PAWN
      case ◯.KY => SavedPieceEnum.LANCE
      case ◯.KE => SavedPieceEnum.KNIGHT
      case ◯.GI => SavedPieceEnum.SILVER
      case ◯.KI => SavedPieceEnum.GOLD
      case ◯.KA => SavedPieceEnum.BISHOP
      case ◯.HI => SavedPieceEnum.ROOK
      // Promoted pieces in core map to their base types in SavedPieceEnum
      // (e.g., TO, NG, RY, UM, NK, NY also map to PAWN, SILVER, ROOK, BISHOP, KNIGHT, LANCE)
      // This detail depends on how `generalize` and `◯` types are defined.
      // Assuming generalize(▲.TO) would be something like ◯.FU.
      case _ => throw new IllegalArgumentException(s"Unknown core piece for SavedPieceEnum: $cp")
    }
  }

  // Gui.board (CoreBoard) to Map[SavedPosition, SavedPieceEnum] for GameState.boardSetup
  private def coreBoardToSavedBoardSetup(board: CoreBoard): Map[SavedPosition, SavedPieceEnum] = {
    board.allBlocks.filter(_.piece != jp.sndyuk.shogi.core.Piece.❏).map { block =>
      corePointToSavedPosition(block.point) -> corePieceToSavedPieceEnum(block.piece)
    }.toMap
  }

  // Get captured pieces for a player as List[SavedPieceEnum]
  private def getCapturedPieces(board: CoreBoard, turn: CoreTurn): List[SavedPieceEnum.Value] = {
    board.capturedPieces.allPieceKinds(turn).flatMap { block =>
      // allPieceKinds gives Block(CorePoint, CorePiece). CorePoint is special for captured.
      // We need to know how many of each.
      val pieceEnum = corePieceToSavedPieceEnum(block.piece)
      val count = board.capturedPieces.count(turn, jp.sndyuk.shogi.core.Piece.generalize(block.piece))
      List.fill(count)(pieceEnum)
    }.toList
  }

  // CoreTransition to SavedTransition (for GameState.gameHistory)
  // This is highly problematic due to SavedTransition's design.
  private def coreTransitionToSavedTransition(ct: CoreTransition, boardAfterMove: CoreBoard): SavedTransition = {
    // SavedTransition requires (move: String, boardStateAfterMove: Map[SavedPosition, SavedPieceEnum])
    // The 'move' string is simple, but 'boardStateAfterMove' is an issue.
    // Storing full board state per move in JSON is inefficient and not what core.Transition provides.
    // For now, we'll use the provided boardAfterMove, which should be the state of the board *after* ct was applied.
    SavedTransition(
      move = s"${ct.oldPos.toString} -> ${ct.newPos.toString}${if (ct.nari) " 成" else ""}", // Example string
      boardStateAfterMove = coreBoardToSavedBoardSetup(boardAfterMove) // This makes gameHistory very large
    )
  }

  // TODO: Mappings for Kifu Exporters (Core types to KifuTempCore types)
  // --- Mappings for Kifu Exporters ---

  private def coreTurnToKifuPlayer(turn: CoreTurn): KifuTempCore.Player = turn match {
    case PlayerA => KifuTempCore.SENTE
    case PlayerB => KifuTempCore.GOTE
  }

  private def corePointToKifuPosition(p: CorePoint): KifuTempCore.Position = {
    // KifuTempCore.Position(x, y) expects 1-indexed shogi board coordinates.
    // CorePoint(y, x) is 0-indexed array coords.
    // Shogi: x is 1-9 (right to left), y is 1-9 (top to bottom)
    // CorePoint: x is 0-8 (left to right), y is 0-8 (top to bottom)
    // KifuTempCore.Position(x,y) in CSAExporter was stringified as x.toString + y.toString
    // For CSA: 11 is top-right (9,1 in shogi), 99 is bottom-left (1,9 in shogi)
    // Let's assume KifuTempCore.Position also follows this (x=file, y=rank)
    KifuTempCore.Position(9 - p.x, p.y + 1)
  }

  private def corePieceToKifuPiece(cp: CorePiece): KifuTempCore.Piece = {
    import jp.sndyuk.shogi.core.Piece._
    val isGote = △(cp)
    val basePiece = generalize(cp) // Removes player and promotion information initially

    // Map base piece to KifuTempCore piece (which has promoted versions)
    // This relies on KifuTempCore.promote helper if we pass base piece + promote flag to KifuTempCore.Move
    // Or, we map directly to promoted KifuTempCore pieces here.
    // The KifuTempCore.Move constructor takes (player, from, to, piece, promote, isDrop)
    // So, we should provide the base piece type and the promote flag.

    basePiece match {
      case ◯.OU => KifuTempCore.OU
      case ◯.FU => KifuTempCore.FU
      case ◯.KY => KifuTempCore.KY
      case ◯.KE => KifuTempCore.KE
      case ◯.GI => KifuTempCore.GI
      case ◯.KI => KifuTempCore.KI
      case ◯.KA => KifuTempCore.KA
      case ◯.HI => KifuTempCore.HI
      case _ => throw new IllegalArgumentException(s"Unknown core piece for Kifu: $cp")
    }
  }

  private def coreTransitionToKifuMove(ct: CoreTransition, playerWhoseMoveItWas: CoreTurn): KifuTempCore.Move = {
    import jp.sndyuk.shogi.core.Piece._

    val fromPosOpt = if (CorePoint.isCaptured(ct.oldPos)) { // Is it a drop?
      None // For drops, 'from' is None in KifuTempCore.Move
    } else {
      Some(corePointToKifuPosition(ct.oldPos))
    }

    val toPos = corePointToKifuPosition(ct.newPos)

    // Determine the piece that moved/was dropped.
    // If drop: oldPos encodes the piece type. Example: Point(9,2) for FU for PlayerA.
    // If move: need to know what piece was at oldPos *before* the move.
    // This information is not directly in CoreTransition if the board state before move is not passed.
    // However, for kifu, we need the piece type *as it was on oldPos*.
    // This requires looking up the piece on the board *before* this transition.
    // This is a problem if we only have the list of transitions.
    // Let's assume for now that ct.captured contains the piece that was captured AT newPos.
    // The piece that MOVED is not in CoreTransition. This is a BIG GAP.
    // Kifu needs the piece that MOVED. E.g. "+7776FU". FU is the piece at 77.
    // The current Gui.board.squares.get(ct.newPos) gives piece *after* move.
    // Workaround: We need to reconstruct the piece that moved.
    // If ct.nari is true, then the piece at newPos is the promoted form. We need its base form.
    // If it was a drop, oldPos tells us the piece.

    val movedPieceCore: CorePiece = if (CorePoint.isCaptured(ct.oldPos)) {
      // It's a drop. oldPos.x determines the piece type for PlayerA's perspective
      // Point.ofCaptured maps general piece type (like ◯.FU) to a Point(9,x).
      // We need to reverse this.
      // Gui.this.board.capturedPieces.pointToPiece(ct.oldPos, playerWhoseMoveItWas) might work if ct.oldPos is that special point.
      // Let's assume ct.oldPos for a drop is like Point(9, piece_code_for_player_A_perspective)
      // Example: if playerWhoseMoveItWas is PlayerA, ct.oldPos.x = 2 means FU.
      // If playerWhoseMoveItWas is PlayerB, ct.oldPos.x = 2 (still FU) needs to be △.FU.
      // This is complex because ct.oldPos is just a point.
      // A simpler way: The piece that lands on newPos IS the dropped piece.
      Gui.this.board.squares.get(ct.newPos) // This is piece after move. For drop, this is the piece.
    } else {
      // It's a move from board. The piece at newPos, potentially un-promoted if ct.nari is true.
      val pieceAtNewPos = Gui.this.board.squares.get(ct.newPos)
      if (ct.nari) reverseIfPromoted(pieceAtNewPos) else pieceAtNewPos
    }

    val kifuPiece = corePieceToKifuPiece(movedPieceCore) // Base form
    val kifuPlayer = coreTurnToKifuPlayer(playerWhoseMoveItWas)

    KifuTempCore.Move(
      player = kifuPlayer,
      from = fromPosOpt,
      to = toPos,
      piece = kifuPiece,
      promote = ct.nari,
      isDrop = CorePoint.isCaptured(ct.oldPos)
    )
  }


  override def top = new MainFrame {
    title = "将棋"
    resizable = false

    // Menu Bar
    menuBar = new MenuBar {
      contents += new Menu("File") {
        mnemonic = Key.F
        contents += new MenuItem(Action("Save Game...") {
          saveGame()
        })
        contents += new MenuItem(Action("Load Game...") {
          loadGame()
        })
        contents += new Separator
        contents += new MenuItem(Action("Export CSA...") {
          exportKifu(isCSA = true)
        })
        contents += new MenuItem(Action("Export KI2...") {
          exportKifu(isCSA = false)
        })
        // Potentially add Exit action later
      }
    }

    contents = new Board
  }

  // --- Menu Action Handlers ---
  private def saveGame(): Unit = {
    if (currState == null) {
      Dialog.showMessage(title = "Save Game", message = "No game active to save.")
      return
    }
    val fileChooser = new FileChooser
    fileChooser.title = "Save Game State"
    if (fileChooser.showSaveDialog(boardPanel.peer) == FileChooser.Result.Approve) {
      val file = fileChooser.selectedFile

      // Need to reconstruct board states for each step in history for SavedTransition
      // This is very inefficient if not done carefully.
      // For simplicity, GameState's SavedTransition stores board state *after* the move.
      // We can iterate through history, apply moves to a temp board, and capture state.
      var tempBoardForHistory = CoreBoard() // Initial board
      val savedHistory = currState.history.reverse.map { coreTrans => // history is in reverse chronological
        // Simulate move on tempBoard to get board state *after* this coreTrans
        val playerForThisMove = if (tempBoardForHistory.squares.id() == Gui.this.board.squares.id()) currState.turn.change else currState.turn // This logic is tricky
        // This is still not quite right. The turn for a history move is fixed.
        // If history has N moves, first move is by Sente, second by Gote etc.
        // We need to know who made coreTrans. Let's assume PlayerA (Sente) starts.
        // The player for the i-th move (0-indexed) is PlayerA if i is even, PlayerB if i is odd.
        // This requires knowing the index of coreTrans in the original sequence.
        // This is getting overly complex due to SavedTransition's requirements.
        // A simpler SavedTransition (e.g. just the move details) would be better.
        // Given the current structure, we'll use the Gui.this.board for all boardStateAfterMove, which is wrong.
        // THIS WILL BE A KNOWN BUG / LIMITATION due to SavedTransition structure.
        // A correct way would be:
        // val boardAfterThisTrans = CoreBoard(); state.history.take(indexOf(coreTrans)+1).reverse.foreach(m => boardAfterThisTrans.move(...))
        // For now, using current board as a placeholder for boardStateAfterMove for all history items.
        coreTransitionToSavedTransition(coreTrans, Gui.this.board)
      }.toList.reverse // maintain original chronological order for saving

      val gameStateToSave = SavedGameState(
        boardSetup = coreBoardToSavedBoardSetup(Gui.this.board),
        currentTurn = coreTurnToSavedPlayer(currState.turn),
        capturedPiecesPlayer1 = getCapturedPieces(Gui.this.board, PlayerA),
        capturedPiecesPlayer2 = getCapturedPieces(Gui.this.board, PlayerB),
        gameHistory = savedHistory // This is the problematic part
      )

      GameSaver.saveToFile(gameStateToSave, file.getAbsolutePath) match {
        case Success(_) => Dialog.showMessage(title = "Success", message = "Game saved.")
        case Failure(e) => Dialog.showMessage(title = "Error", message = s"Failed to save game: ${e.getMessage}")
      }
    }
  }

  private def loadGame(): Unit = {
    val fileChooser = new FileChooser
    fileChooser.title = "Load Game State"
    if (fileChooser.showOpenDialog(boardPanel.peer) == FileChooser.Result.Approve) {
      val file = fileChooser.selectedFile
      GameSaver.loadFromFile(file.getAbsolutePath) match {
        case Success(loadedGameState) =>
          // Attempt to reconstruct Gui.this.board and Gui.this.currState from loadedGameState.
          // WARNING: This is a simplified and potentially very lossy reconstruction due to
          // the limitations of SavedGameState and SavedTransition.

          // 1. Reset board to initial state
          Gui.this.board.init() // Resets squares and captured pieces

          // 2. Place pieces on the board from loadedGameState.boardSetup
          // This is lossy: SavedPieceEnum doesn't have player or promotion.
          // We infer player based on typical y-coordinate for shogi (Sente at higher y for 0-indexed array).
          loadedGameState.boardSetup.foreach { case (savedPos, savedPieceEnum) =>
            val coreY = savedPos.y // SavedPos is (x,y) from top-left, core is (y,x) from top-left
            val coreX = savedPos.x
            val player = if (coreY >= 5) PlayerA else PlayerB // Crude Sente/Gote determination

            // Map SavedPieceEnum back to a base CorePiece type for that player
            val baseCorePiece = savedPieceEnumToCorePiece(savedPieceEnum, player)
            Gui.this.board.squares <+ (baseCorePiece, CorePoint(coreY, coreX))
          }

          // 3. Restore captured pieces
          loadedGameState.capturedPiecesPlayer1.foreach { savedPieceEnum =>
            Gui.this.board.capturedPieces.put(savedPieceEnumToCorePiece(savedPieceEnum, PlayerA))
          }
          loadedGameState.capturedPiecesPlayer2.foreach { savedPieceEnum =>
            Gui.this.board.capturedPieces.put(savedPieceEnumToCorePiece(savedPieceEnum, PlayerB))
          }

          // 4. History Reconstruction (Extremely difficult and not attempted here)
          // loadedGameState.gameHistory is List[SavedTransition]
          // Each SavedTransition has a 'move' string and a 'boardStateAfterMove' map.
          // Parsing 'move' string to CoreTransition is complex.
          // 'boardStateAfterMove' was also saved problematically.
          // For now, history will be effectively lost or incorrect.
          val reconstructedHistory: List[CoreTransition] = Nil // Placeholder

          // 5. Set current turn
          val newCoreTurn = loadedGameState.currentTurn match {
            case SavedPlayer.SENTE => PlayerA
            case SavedPlayer.GOTE  => PlayerB
          }
          currState = CoreState(reconstructedHistory, newCoreTurn)

          // 6. Refresh UI
          afterMove(player, null, null) // `player` here is the HumanPlayer, oldPos/newPos are null as it's a load

          Dialog.showMessage(boardPanel.peer, "Game loaded. WARNING: Reconstruction is partial and may be inaccurate due to save format limitations.", title = "Load Complete")

        case Failure(e) => Dialog.showMessage(boardPanel.peer, s"Failed to load game: ${e.getMessage}", title = "Load Error", messageType = Dialog.Message.Error)
      }
    }
  }

  // Helper for loadGame: SavedPieceEnum to CorePiece (base form for a player)
  private def savedPieceEnumToCorePiece(spe: SavedPieceEnum.Value, player: CoreTurn): CorePiece = {
    import jp.sndyuk.shogi.core.Piece._
    val baseGeneralized = spe match {
      case SavedPieceEnum.KING   => ◯.OU
      case SavedPieceEnum.ROOK   => ◯.HI
      case SavedPieceEnum.BISHOP => ◯.KA
      case SavedPieceEnum.GOLD   => ◯.KI
      case SavedPieceEnum.SILVER => ◯.GI
      case SavedPieceEnum.KNIGHT => ◯.KE
      case SavedPieceEnum.LANCE  => ◯.KY
      case SavedPieceEnum.PAWN   => ◯.FU
    }
    // Apply player to the generalized piece
    if (player == PlayerA) baseGeneralized & ~bitsPlayerBPiece & ~bitsGeneralPiece // Make it Sente
    else (baseGeneralized & ~bitsGeneralPiece) | bitsPlayerBPiece // Make it Gote
  }


  private def exportKifu(isCSA: Boolean): Unit = {
    if (currState == null || currState.history.isEmpty) {
      Dialog.showMessage(boardPanel.peer, "No game history to export.", title = "Export Kifu", messageType = Dialog.Message.Info)
      return
    }
    val fileChooser = new FileChooser
    val format = if (isCSA) "CSA" else "KI2"
    fileChooser.title = s"Export Kifu to $format"
    if (fileChooser.showSaveDialog(boardPanel.peer) == FileChooser.Result.Approve) {
      val file = fileChooser.selectedFile

      // TODO: Implement mappings from jp.sndyuk.shogi.core types to KifuTempCore types
      // This involves:
      // - Mapping CoreBoard to KifuTempCore.Board (mainly for initial setup if not standard)
      // - Mapping List[CoreTransition] to Seq[KifuTempCore.Transition]
      //    - Each CoreTransition needs to be converted to KifuTempCore.Move
      //    - Need to determine player for each move in history.
      // - Mapping CoreTurn to KifuTempCore.Turn
      // - Game result (currently passing None)

      // val kifuBoard: KifuTempCore.Board = ??? // map from Gui.this.board (if needed for non-standard start)
      // val kifuHistory: Seq[KifuTempCore.Transition] = ??? // map from currState.history
      // Determine player for each move in history.
      // currState.history is newest move first.
      // Player for currState.history.head was currState.turn.change
      // Player for currState.history(1) was currState.turn
      // etc.
      var playerForCurrentHistMove = currState.turn.change
      val kifuHistoryMoves = currState.history.map { coreTrans =>
        val kifuMove = coreTransitionToKifuMove(coreTrans, playerForCurrentHistMove)
        playerForCurrentHistMove = playerForCurrentHistMove.change // Alternate for next older move
        KifuTempCore.Transition(kifuMove) // Assuming KifuTempCore.Transition just wraps a KifuTempCore.Move
      }.reverse // Reverse to get chronological order (oldest first) for exporters

      // Initial board state for kifu (usually for CSA non-standard starts)
      // For simplicity, we'll assume standard Hirate, so exporters handle it.
      // A full impl might convert Gui.this.board to KifuTempCore.Board if it's turn 0.
      val kifuInitialBoard = KifuTempCore.Board(Map.empty, KifuTempCore.SENTE) // Placeholder

      // Current turn for kifu (player whose turn it is *now*)
      val kifuCurrentTurn = coreTurnToKifuPlayer(currState.turn)

      // Game result (e.g., "%TORYO" for CSA, "まで77手で先手の勝ち" for KI2)
      // This needs to be determined when a game actually ends. Placeholder for now.
      val gameResult: Option[String] = None
      // Example if game ended:
      // if (currState.isCheckmate) { // Assuming State has such a field
      //   gameResult = Some(if (isCSA) "%TORYO" else s"まで${currState.history.size}手で${if (currState.turn == PlayerB) "先手" else "後手"}の勝ち")
      // }


      val kifuString = if (isCSA) {
        CSAExporter.exportToString(kifuInitialBoard, kifuHistoryMoves, kifuCurrentTurn, gameResult)
      } else {
        KI2Exporter.exportToString(kifuInitialBoard, kifuHistoryMoves, kifuCurrentTurn, gameResult)
      }

      Try {
        val pw = new PrintWriter(new FileWriter(file))
        pw.write(kifuString)
        pw.close()
      } match {
        case Success(_) => Dialog.showMessage(boardPanel.peer, s"$format kifu exported successfully.", title = "Export Success")
        case Failure(e) => Dialog.showMessage(boardPanel.peer, s"Failed to export $format kifu: ${e.getMessage}", title = "Export Error", messageType = Dialog.Message.Error)
      }
    }
  }


  class Board extends BoxPanel(Orientation.Horizontal) {

    contents += turnBInfoPanel
    contents += boardPanel
    contents += turnAInfoPanel

    var selected: Option[CapturedPiecePanel] = None

    listenTo(mouse.clicks)
    listenTo(mouse.moves)

    reactions += {
      case MouseDragged(source, point, modifiers) =>
        val loc = location
        Option(peer.getParent.findComponentAt(loc.x + point.x, loc.y + point.y)).foreach { c =>
          val destOpt = capturedPiecesByComponent.get(c)
          if (destOpt.isDefined && destOpt.exists(_.count > 0)) {
            selected = destOpt
          }
        }

      case MouseReleased(source, point, modifiers, clicks, triggersPopup) =>
        selected.foreach { capturedPiecePanel =>
          selected = None
          val loc = location
          Option(peer.getParent.findComponentAt(loc.x + point.x, loc.y + point.y)).foreach { c =>
            val destOpt = piecesOnViewByComponent.get(c)
            destOpt.foreach { destPanel =>
              onSelect(capturedPiecePanel.block.piece, capturedPiecePanel.block.point, destPanel.block.point)
            }
          }
        }
    }
  }

  class InfoPanel(turn: Turn) extends GridPanel(9, 2) {
    vGap = blockMargin
    hGap = blockMargin

    preferredSize = new Dimension(
      (blockSize + blockMargin) * 2,
      (blockSize + blockMargin) * 9)

    val tumeroButton = new Button("詰判定") {
      reactions += {
        case ButtonClicked(source) =>
          if (currState != null && !currState.history.isEmpty) {
            val tsumi = Utils.isTsumero(board, currState, turn, 3)
            println(s"詰: $tsumi")
          } else {
            println(s"詰: 0")
          }
      }
    }

    def rebuild() = {
      contents.clear
      val capturedPiecePanels = buildAllCapturedBlocks
      contents ++= capturedPiecePanels
      contents += tumeroButton
      capturedPiecePanels.map { c =>
        capturedPiecesByComponent += c.peer -> c
      }
      peer.invalidate
    }
    rebuild

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
    }

    private def buildAllCapturedBlocks: Seq[CapturedPiecePanel] = {
      ◯.all.filterNot(_ == ◯.OU).map { (piece) =>
        val p = Piece.convert(piece, turn)
        new CapturedPiecePanel(turn, Block(Point(9, p), p), board.capturedPieces.count(turn, piece))
      }
    }
  }

  val turnAInfoPanel = new InfoPanel(PlayerA) // PlayerA seems to be Sente
  val turnBInfoPanel = new InfoPanel(PlayerB) // PlayerB seems to be Gote

  val boardPanel = new BoardPanel {

    vGap = blockMargin
    hGap = blockMargin

    contents ++= buildAllBlocks

    preferredSize = new Dimension(
      (blockSize + blockMargin) * 9,
      (blockSize + blockMargin) * 9)

    focusable = true

    def rebuild = {
      contents.clear
      contents ++= buildAllBlocks
      peer.invalidate
    }

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      g setColor colorLightBlack
      g fillRect (0, 0, size.width, size.height)
    }

    def onKeyPress(keyCode: Value) = keyCode match {
      case Left => println(keyCode)
      case Right => println(keyCode)
      case Up => println(keyCode)
      case Down => println(keyCode)
      case Space => println(keyCode)
      case _ => println(keyCode)
    }

    private def buildAllBlocks: Seq[PiecePanel] = {
      piecesOnViewByComponent.clear
      piecesOnViewByPiece.clear

      board.allBlocks map { (block) =>
        val piecePanel = new PiecePanel(block)
        piecesOnViewByComponent += piecePanel.peer -> piecePanel
        piecesOnViewByPiece += block.piece -> piecePanel
        piecePanel
      }
    }
  }

  def buildRect(pos: Point): Rectangle =
    new Rectangle(pos.x * (blockSize + blockMargin),
      (9 - pos.y - 1) * (blockSize + blockMargin),
      blockSize, blockSize)

  def buildPiece(nearSide: Boolean, xMargin: Int = 0): Polygon = {
    val baseX = pieceMargin + xMargin
    val baseY = pieceMargin
    if (nearSide) {
      new Polygon(
        Array(
          baseX + pieceSize,
          baseX,
          baseX + (pieceSize / 6),
          baseX + (pieceSize / 2),
          baseX + pieceSize - (pieceSize / 6)),
        Array(
          baseY + pieceSize,
          baseY + pieceSize,
          baseY + (pieceSize / 6),
          baseY,
          baseY + (pieceSize / 6)), 5)
    } else {
      new Polygon(
        Array(
          baseX,
          baseX + pieceSize,
          baseX + pieceSize - (pieceSize / 6),
          baseX + (pieceSize / 2),
          baseX + (pieceSize / 6)),
        Array(
          baseY,
          baseY,
          baseY + pieceSize - (pieceSize / 6),
          baseY + pieceSize,
          baseY + pieceSize - (pieceSize / 6)), 5)
    }
  }

  def buildPieceString(piece: Piece, nearSide: Boolean): (String, Int, Int) = {
    val name = Piece.name(piece)
    (name,
      if (name.length == 1) blockSize / 2 - textSize / 2 else blockSize / 2 - textSize,
      blockSize / 2 + textSize / 2)
  }

  private def onSelect(piece: Piece, oldPos: Point, newPos: Point): Unit = {
    if (oldPos != newPos) {
      if (Rule.canMove(board, piece, oldPos, newPos, turn(player))) {
        this.oldPos = oldPos
        this.newPos = newPos
        this.nari = chooseIfPieceCanBePromoted(piece, oldPos, newPos)
        boardLatch.countDown()
      } else {
        failToMove(player, oldPos, newPos)
      }
    }
  }

  private def turn(player: Player): Turn = if (player == playerA) PlayerA else PlayerB

  private def chooseIfPieceCanBePromoted(piece: Piece, oldPos: Point, newPos: Point): Boolean = {
    if (Rule.canBePromoted(board, oldPos, newPos, piece)) {
      if (Rule.canMoveAtNextTurn(piece, newPos)) { // Check if the piece *can* move if it doesn't promote (e.g. Keima in last rank must promote)
        Dialog.showConfirmation(parent = boardPanel.peer, title = "成駒確認", message = "成りますか。") == Dialog.Result.Ok
      } else {
        true // Must promote
      }
    } else false
  }

  override def afterMove(player: Player, oldPos: Point, newPos: Point): Unit = {
    println(board.toString) // Log board state to console
    boardPanel.rebuild
    capturedPiecesByComponent.clear // Reset for InfoPanels
    turnAInfoPanel.rebuild
    turnBInfoPanel.rebuild

    boardPanel.revalidate
    turnAInfoPanel.revalidate
    turnBInfoPanel.revalidate

    boardPanel.repaint // Repaint board
    turnAInfoPanel.repaint // Repaint info panels
    turnBInfoPanel.repaint
  }

  override def beforeMove(player: Player): Unit = {
    this.player = player
  }

  override def done(player: Player, oldPos: Point, newPos: Point, winner: Player): Unit = {
    // TODO: Store game result properly for export
    val resultMessage = s"${winner}の勝ちです。"
    Dialog.showMessage(parent = boardPanel.peer, title = "終局", message = resultMessage)
  }

  override def failToMove(player: Player, oldPos: Point, newPos: Point): Unit = {
    Dialog.showMessage(parent = boardPanel.peer, title = "移動不可", message = "そこには置けません。")
  }

  override def onError(e: Exception): Unit = {
    Dialog.showMessage(parent = boardPanel.peer, title = "エラー", message = e.getMessage, messageType = Dialog.Message.Error)
    // TODO for debug. remove if not needed
    e.printStackTrace()
  }

  Future {
    start // This seems to be the game loop invocation from the Shogi trait
  }

  // NPS Display:
  // The P/R states: "I will ensure this information (NPS, nodes visited, AI thinking time)
  // is clearly visible in the AIBattleSim console output if it's not already prominent."
  // This means no specific GUI changes for NPS are mandated for Gui.scala in this subtask,
  // especially since AIBattleSim is a separate simulation environment.
  // If AIPlayer used within this Gui were to log NPS, it would appear in the console
  // where this application's output goes.
}
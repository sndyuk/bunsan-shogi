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
import scala.swing.event.{Key, KeyPressed} // Specific import for Key, KeyPressed for onKeyPress
import scala.swing.event.MouseDragged
import scala.swing.event.MouseEntered
import scala.swing.event.MouseReleased

import jp.sndyuk.shogi.core.Block
// import jp.sndyuk.shogi.core.Board // Unused import
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

// Import alias for CoreBoard at the top
import jp.sndyuk.shogi.core.{Board => CoreBoard}
// NEW IMPORTS for GameStateMapper and ShogiGameService
import jp.sndyuk.shogi.core.GameStateMapper // GameStateMapper object
// import jp.sndyuk.shogi.core.SimplePiece // Unused import
// import jp.sndyuk.shogi.core.{Player => CorePlayer} // Unused import
import jp.sndyuk.shogi.core.SimpleTransition // For SimpleTransition case class
import jp.sndyuk.shogi.core.ShogiGameService
import jp.sndyuk.shogi.kifu.KifuMapper // NEW Import for KifuMapper
// Corrected import for KifuTempCore
import jp.sndyuk.shogi.kifu.{TempCore => KifuTempCore}


case class BoardView(blocks: Seq[Block], piecesOfPlayerA: List[Block], piecesOfPlayerB: List[Block])

abstract class BoardPanel extends GridPanel(9, 9) {
  def rebuild(): Unit
}

object Gui extends SimpleSwingApplication with Shogi {

  // NEW: ShogiGameService instance
  val shogiGameService = new ShogiGameService()

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

  var board = CoreBoard() // This should now use the alias defined at the top

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
    // Player => SavedPlayer, // Will use CorePlayer.Player (aliased)
    // Position => SavedPosition, // Unused import
    // Piece => SavedPieceEnum, // Will use SimplePiece.SimplePieceType
    // Transition => SavedTransition // Will use SimpleTransition
    PieceInfo // Case class for board setup serialization
  }
  import jp.sndyuk.shogi.kifu.{CSAExporter, KI2Exporter}
  // Kifu exporters currently use their own TempCore, we need to map to that.
  // Ideally, exporters would use jp.sndyuk.shogi.core types directly or a shared Kifu model.
  // This import is already corrected/covered by the one at the top of the file.
  // import jp.sndyuk.shogi.kifu.{TempCore => KifuTempCore} // Corrected path

  // Core types from the game logic
  // The CoreBoard alias is moved to the top. Other specific aliases can remain if used locally.
  import jp.sndyuk.shogi.core.{
    // Board => CoreBoard, // Alias moved to top
    Piece => CorePiece, // Alias for jp.sndyuk.shogi.core.Piece
    Point => CorePoint, // Alias for jp.sndyuk.shogi.core.Point
    State => CoreState, // Alias for jp.sndyuk.shogi.core.State
    // Transition => CoreTransition, // Unused import
    Turn => CoreTurn, // Alias for jp.sndyuk.shogi.core.Turn
    PlayerA, // Represents Sente in core logic (jp.sndyuk.shogi.core.PlayerA)
    PlayerB // Represents Gote in core logic (jp.sndyuk.shogi.core.PlayerB)
    // SavedSimpleTransition, // Removed, using SimpleTransition
    // SavedPieceEnum,      // Removed, using SimplePiece.SimplePieceType
    // SavedPlayerEnum      // Removed, using CorePlayer.Player
  }
  import scala.util.{Try, Success, Failure}
  import java.io.{PrintWriter, FileWriter} // Re-add import for file operations, remove File

  // Local mapping functions are removed as per refactoring plan. GameStateMapper will be used.
  // --- End of removed local mapping functions for save/load ---

  // Kifu related local mapping functions will also be removed and KifuMapper used.
  // --- End of removed local Kifu mapping functions ---


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
    if (fileChooser.showSaveDialog(boardPanel) == FileChooser.Result.Approve) { // Pass boardPanel directly
      val file = fileChooser.selectedFile

      // History Mapping (adapted from ShogiGameService.getGameState)
      val gameHistoryMapped: List[SimpleTransition] = { // Use SimpleTransition
        if (currState.history.isEmpty) {
          Nil
        } else {
          // Determine initial board state for history replay.
          // This is complex. For now, assume standard CoreBoard() was the start.
          // This will be incorrect if the game was loaded from a custom state.
          val tempBoard = CoreBoard() // Changed var to val

          // Determine the starting player of the game based on current state and history length
          val gameStartingTurn = if (currState.history.length % 2 == 0) {
            currState.turn
          } else {
            currState.turn.change
          }

          currState.history.reverse.zipWithIndex.map { case (coreTrans, index) =>
            val boardBeforeThisMove = tempBoard.copy()
            val playerForThisTransition = if (index % 2 == 0) gameStartingTurn else gameStartingTurn.change

            val dummyStateForHistoryMove = CoreState(Nil, playerForThisTransition)
            tempBoard.move(dummyStateForHistoryMove, coreTrans.oldPos, coreTrans.newPos, validation = false, nari = coreTrans.nari)
            // tempBoard is now boardAfterThisMove

            GameStateMapper.coreTransitionToSimpleTransition(coreTrans, boardBeforeThisMove, tempBoard)
          }.toList // Already in chronological order due to .reverse.map
        }
      }

      val capturedSente = jp.sndyuk.shogi.core.Piece.◯.all.flatMap { generalizedPiece => // Use fully qualified Piece
        val count = Gui.this.board.capturedPieces.count(PlayerA, generalizedPiece)
        List.fill(count)(
          GameStateMapper.corePieceToSimplePieceTypeAndPlayer(generalizedPiece) match {
            case Some((spt, _, _)) => spt // spt is SimplePiece.SimplePieceType
            case None => throw new IllegalStateException(s"Cannot map core captured piece $generalizedPiece")
          }
        )
      }.toList

      val capturedGote = jp.sndyuk.shogi.core.Piece.◯.all.flatMap { generalizedPiece => // Use fully qualified Piece
        val count = Gui.this.board.capturedPieces.count(PlayerB, generalizedPiece)
        List.fill(count)(
          GameStateMapper.corePieceToSimplePieceTypeAndPlayer(generalizedPiece) match {
            case Some((spt, _, _)) => spt // spt is SimplePiece.SimplePieceType
            case None => throw new IllegalStateException(s"Cannot map core captured piece $generalizedPiece")
          }
        )
      }.toList

      val gameStateToSave = SavedGameState(
        boardSetup = GameStateMapper.coreBoardToBoardSetup(Gui.this.board), // Returns Map[String, PieceInfo]
        currentTurn = GameStateMapper.coreTurnToPlayer(currState.turn), // Returns CorePlayer.Player
        capturedPiecesPlayer1 = capturedSente, // List[SimplePiece.SimplePieceType]
        capturedPiecesPlayer2 = capturedGote, // List[SimplePiece.SimplePieceType]
        gameHistory = gameHistoryMapped,
        evaluationScore = 0
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
    if (fileChooser.showOpenDialog(boardPanel) == FileChooser.Result.Approve) { // Pass boardPanel directly
      val file = fileChooser.selectedFile
      GameSaver.loadFromFile(file.getAbsolutePath) match {
        case Success(loadedGameState) =>
          // Prepare data for ShogiGameService.startNewGame
          // loadedGameState.boardSetup is already Map[String, PieceInfo]
          // PieceInfo has pieceType: SimplePiece.SimplePieceType, player: CorePlayer.Player, isPromoted: Boolean
          // ShogiGameService expects Option[Map[String, PieceInfo]]
          val initialBoardSetupForService: Map[String, PieceInfo] = loadedGameState.boardSetup

          // Call ShogiGameService to set its internal state
          // Note: loadedGameState.currentTurn is CorePlayer.Player, which matches what ShogiGameService expects for firstPlayer.
          shogiGameService.startNewGame(
            initialBoardSetup = Some(initialBoardSetupForService), // Pass the correctly typed map
            initialSenteCaptured = loadedGameState.capturedPiecesPlayer1,
            initialGoteCaptured = loadedGameState.capturedPiecesPlayer2,
            firstPlayer = loadedGameState.currentTurn
          )

          // Update Gui's internal board and state from the service's state
          // This assumes shogiGameService.board and .currentState are accessible (e.g. public val)
          Gui.this.board = shogiGameService.board.copy()
          Gui.this.currState = shogiGameService.currentState.copy()

          // Refresh UI
          // Determine which HumanPlayer object to pass based on current turn.
          // The 'player' for afterMove is the one whose turn it *was* or who is active.
          // After loading, it's start of new currentTurn.
          val currentPlayerObject = if (Gui.this.currState.turn == PlayerA) Gui.this.playerA else Gui.this.playerB
          afterMove(currentPlayerObject, null, null)

          Dialog.showMessage(boardPanel, "Game loaded via ShogiGameService.", title = "Load Complete") // Pass boardPanel directly

        case Failure(e) => Dialog.showMessage(boardPanel, s"Failed to load game: ${e.getMessage}", title = "Load Error", messageType = Dialog.Message.Error) // Pass boardPanel directly
      }
    }
  }

  // Removed old savedPieceEnumToCorePiece helper function

  private def exportKifu(isCSA: Boolean): Unit = {
    if (currState == null || currState.history.isEmpty) {
      Dialog.showMessage(boardPanel, "No game history to export.", title = "Export Kifu", messageType = Dialog.Message.Info) // Pass boardPanel directly
      return
    }
    val fileChooser = new FileChooser
    val format = if (isCSA) "CSA" else "KI2"
    fileChooser.title = s"Export Kifu to $format"
    if (fileChooser.showSaveDialog(boardPanel) == FileChooser.Result.Approve) { // Pass boardPanel directly
      val file = fileChooser.selectedFile

      // Determine initial board state for history replay for Kifu export.
      // Assuming game started from standard CoreBoard() if not loaded otherwise.
      // This is a simplification; a robust solution would track the true initial state.
      val tempBoardForKifu = CoreBoard() // Changed var to val

      // Determine the starting player of the game.
      val gameStartingTurnForKifu = if (currState.history.length % 2 == 0) {
        currState.turn
      } else {
        currState.turn.change
      }

      val kifuHistoryMoves = currState.history.reverse.zipWithIndex.map { case (coreTrans, index) =>
        val boardBeforeThisMove = tempBoardForKifu.copy()
        val playerForThisMove = if (index % 2 == 0) gameStartingTurnForKifu else gameStartingTurnForKifu.change

        val kifuMove = KifuMapper.coreTransitionToKifuMove(coreTrans, playerForThisMove, boardBeforeThisMove)

        // Apply move to tempBoardForKifu to get state for the next iteration's boardBeforeThisMove
        val dummyState = CoreState(Nil, playerForThisMove)
        tempBoardForKifu.move(dummyState, coreTrans.oldPos, coreTrans.newPos, false, coreTrans.nari)

        KifuTempCore.Transition(kifuMove) // Assuming KifuTempCore.Transition just wraps a KifuTempCore.Move
      }.toList // Already chronological due to .reverse.map

      // Initial board state for kifu (usually for CSA non-standard starts)
      // Using a placeholder as KifuMapper.coreBoardToKifuBoard is not implemented.
      val kifuInitialBoard = KifuTempCore.Board(Map.empty, KifuTempCore.SENTE)

      // Current turn for kifu (player whose turn it is *now*)
      val kifuCurrentTurn = KifuMapper.coreTurnToKifuPlayer(currState.turn)

      val gameResult: Option[String] = None // Placeholder for game result

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
        case Success(_) => Dialog.showMessage(boardPanel, s"$format kifu exported successfully.", title = "Export Success") // Pass boardPanel directly
        case Failure(e) => Dialog.showMessage(boardPanel, s"Failed to export $format kifu: ${e.getMessage}", title = "Export Error", messageType = Dialog.Message.Error) // Pass boardPanel directly
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
    listenTo(keys) // Need to listen to key events

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

    reactions += { // Add reaction for key presses
      case kp: KeyPressed => onKeyPress(kp.key)
    }

    def onKeyPress(keyCode: Key.Value): Unit = keyCode match { // Use Key.Value and Unit return type
      case Key.Left => println(keyCode)
      case Key.Right => println(keyCode)
      case Key.Up => println(keyCode)
      case Key.Down => println(keyCode)
      case Key.Space => println(keyCode)
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

  private def turn(p: jp.sndyuk.shogi.player.Player): CoreTurn = if (p == playerA) PlayerA else PlayerB // Use aliased CoreTurn, clarify jp.sndyuk.shogi.player.Player

  private def chooseIfPieceCanBePromoted(piece: CorePiece, oldPos: CorePoint, newPos: CorePoint): Boolean = { // Use aliased types
    if (Rule.canBePromoted(board, oldPos, newPos, piece)) {
      if (Rule.canMoveAtNextTurn(piece, newPos)) { // Check if the piece *can* move if it doesn't promote (e.g. Keima in last rank must promote)
        Dialog.showConfirmation(parent = boardPanel, title = "成駒確認", message = "成りますか。") == Dialog.Result.Ok // Pass boardPanel directly
      } else {
        true // Must promote
      }
    } else false
  }

  override def afterMove(shogiPlayer: jp.sndyuk.shogi.player.Player, oldPos: CorePoint, newPos: CorePoint): Unit = { // Use aliased types, clarify jp.sndyuk.shogi.player.Player
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

  override def beforeMove(shogiPlayer: jp.sndyuk.shogi.player.Player): Unit = { // Clarify jp.sndyuk.shogi.player.Player
    this.player = shogiPlayer
  }

  override def done(shogiPlayer: jp.sndyuk.shogi.player.Player, oldPos: CorePoint, newPos: CorePoint, winner: jp.sndyuk.shogi.player.Player): Unit = { // Use aliased types, clarify jp.sndyuk.shogi.player.Player
    // TODO: Store game result properly for export
    val resultMessage = s"${winner}の勝ちです。"
    Dialog.showMessage(parent = boardPanel, title = "終局", message = resultMessage) // Pass boardPanel directly
  }

  override def failToMove(shogiPlayer: jp.sndyuk.shogi.player.Player, oldPos: CorePoint, newPos: CorePoint): Unit = { // Use aliased types, clarify jp.sndyuk.shogi.player.Player
    Dialog.showMessage(parent = boardPanel, title = "移動不可", message = "そこには置けません。") // Pass boardPanel directly
  }

  override def onError(e: Exception): Unit = { // Clarify Player type if used here, though not directly in method signature
    Dialog.showMessage(parent = boardPanel, title = "エラー", message = e.getMessage, messageType = Dialog.Message.Error) // Pass boardPanel directly
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
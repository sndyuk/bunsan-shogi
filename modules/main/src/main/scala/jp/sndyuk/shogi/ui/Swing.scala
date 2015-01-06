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
import scala.swing.Color
import scala.swing.Dialog
import scala.swing.GridPanel
import scala.swing.MainFrame
import scala.swing.Orientation
import scala.swing.Panel
import scala.swing.SimpleSwingApplication
import scala.swing.event.Key.Down
import scala.swing.event.Key.Left
import scala.swing.event.Key.Right
import scala.swing.event.Key.Space
import scala.swing.event.Key.Up
import scala.swing.event.Key.Value
import scala.swing.event.MouseClicked
import scala.swing.event.MouseDragged
import scala.swing.event.MouseReleased
import jp.sndyuk.shogi.core.Block
import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.Piece
import jp.sndyuk.shogi.core.Piece
import jp.sndyuk.shogi.core.Piece.generalize
import jp.sndyuk.shogi.core.Piece.{ ▲ => ▲ }
import jp.sndyuk.shogi.core.Piece.{ △ => △ }
import jp.sndyuk.shogi.core.Piece.{ ◯ => ◯ }
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
import scala.swing.event.MouseEntered

case class BoardView(blocks: Seq[Block], piecesOfPlayerA: Seq[Block], piecesOfPlayerB: Seq[Block])

abstract class BoardPanel extends GridPanel(9, 9) {
  def rebuild
}

object Swing extends SimpleSwingApplication with Shogi {

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
      Transition(oldPos, newPos, nari, board.pieceOnBoardNotEmpty(newPos))
    }
  }

  val playerA = new HumanPlayer("playerA", board, commandReader, false)
  val playerB = new HumanPlayer("playerB", board, commandReader, false)
  //  val playerB = new AIPlayer()

  @volatile private var player: Player = playerA
  @volatile private var oldPos: Point = (0, 0)
  @volatile private var newPos: Point = (0, 0)
  @volatile private var nari = false

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

    def enterDragOver(block: Block) {
      potantiallyRelpaceWith = Option(block)
      repaint
    }

    def exitDragOver {
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

    override def paintComponent(g: Graphics2D) {
      super.paintComponent(g)

      def drawPiece {
        def $(block: Block) = {
          g fill buildPiece(▲(block.piece))
          val (s, x, y) = buildPieceString(block.piece, ▲(block.piece))

          val orig = g.getTransform
          if (△(block.piece)) {
            g.rotate(Math.PI, blockSize / 2, blockSize / 2)
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

    override def paintComponent(g: Graphics2D) {
      super.paintComponent(g)

      def drawPiece {
        def $(block: Block) = {
          val xMergin = if (turnA) 0 else blockSize + blockMargin
          g fill buildPiece(turnA, xMergin)
          val (s, x, y) = buildPieceString(block.piece, turnA)

          val orig = g.getTransform
          if (turn == PlayerB) {
            g.rotate(Math.PI, blockSize / 2, blockSize / 2)
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

  override def top = new MainFrame {
    title = "将棋"
    resizable = false

    contents = new Board
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

    var capturedPiecePanels = buildAllCapturedBlocks
    contents ++= capturedPiecePanels
    capturedPiecePanels.map { c =>
      capturedPiecesByComponent += c.peer -> c
    }

    def rebuild = {
      contents.clear
      capturedPiecePanels = buildAllCapturedBlocks
      contents ++= capturedPiecePanels
      capturedPiecePanels.map { c =>
        capturedPiecesByComponent += c.peer -> c
      }
      peer.invalidate
    }

    override def paintComponent(g: Graphics2D) {
      super.paintComponent(g)
    }

    private def buildAllCapturedBlocks: Seq[CapturedPiecePanel] = {
      ◯.all.filterNot(_ == ◯.OU).map { (piece) =>
        val p = Piece.invert(piece, turn)
        new CapturedPiecePanel(turn, Block(Point(9, p), p), board.capturedPieces.count(turn, piece))
      }
    }
  }

  val turnAInfoPanel = new InfoPanel(PlayerA)
  val turnBInfoPanel = new InfoPanel(PlayerB)

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

    override def paintComponent(g: Graphics2D) {
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

  private def onSelect(piece: Piece, oldPos: Point, newPos: Point) {
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
      if (Rule.canMoveAtNextTurn(piece, newPos)) {
        Dialog.showConfirmation(title = "成駒確認", message = "成りますか。") == Dialog.Result.Yes
      } else {
        true
      }
    } else false
  }

  override def afterMove(player: Player, oldPos: Point, newPos: Point) {
    println(board.toString)
    boardPanel.rebuild
    capturedPiecesByComponent.clear
    turnAInfoPanel.rebuild
    turnBInfoPanel.rebuild

    boardPanel.revalidate
    turnAInfoPanel.revalidate
    turnBInfoPanel.revalidate

    boardPanel.repaint
  }

  override def beforeMove(player: Player) {
    this.player = player
  }

  override def done(player: Player, oldPos: Point, newPos: Point, winner: Player) {
    Dialog.showMessage(title = "終了", message = "終わりです。")
  }

  override def failToMove(player: Player, oldPos: Point, newPos: Point) {
    Dialog.showMessage(title = "移動不可", message = "置けません。")
  }

  override def onError(e: Exception) {
    Dialog.showMessage(title = "エラー", message = e.getMessage)
    // TODO for debug. remove
    e.printStackTrace
  }

  Future {
    start
  }
}
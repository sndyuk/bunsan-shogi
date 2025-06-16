package jp.sndyuk.shogi.ui

import jp.sndyuk.shogi.core._
import org.scalatest.funsuite.AnyFunSuite

class GuiTest extends AnyFunSuite {

  test("Initial piece placement on GUI") {
    // Initialize the GUI
    val gui = Gui

    // Allow GUI to initialize
    Thread.sleep(1000)

    // Get the board representation from the GUI
    val boardPanel = gui.boardPanel
    val piecePanels = boardPanel.contents.collect { case pp: gui.PiecePanel => pp }

    // Verify piece positions
    assert(piecePanels.length == 81, "Board should have 81 squares")

    // Verify some specific pieces
    // Note: This requires accessing the `block` and `piece` within `PiecePanel`.
    // The exact structure might need adjustment based on PiecePanel's implementation.

    // Example: Verify King's initial position for Player A (Sente)
    val kingAPanel = piecePanels.find(pp => Gui.board.squares.get(pp.block.point) == Piece.▲.OU)
    assert(kingAPanel.isDefined, "Player A's King should be on the board")
    assert(kingAPanel.get.block.point == Point(8, 4), "Player A's King should be at (8, 4)")

    // Example: Verify Player B's (Gote) King's initial position
    val kingBPanel = piecePanels.find(pp => Gui.board.squares.get(pp.block.point) == Piece.△.OU)
    assert(kingBPanel.isDefined, "Player B's King should be on the board")
    assert(kingBPanel.get.block.point == Point(0, 4), "Player B's King should be at (0, 4)")

    // Add more assertions for other pieces as needed
  }
}

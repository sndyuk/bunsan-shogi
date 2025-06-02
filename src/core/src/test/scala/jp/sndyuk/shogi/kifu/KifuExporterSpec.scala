package jp.sndyuk.shogi.kifu

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// Using the TempCore types defined within CSAExporter for Kifu Exporter tests,
// as the exporters are currently based on them.
import jp.sndyuk.shogi.kifu.CSAExporter.TempCore
import jp.sndyuk.shogi.kifu.CSAExporter.TempCore.{Board => KifuBoard, Piece => KifuPiece, Player => KifuPlayer, Position => KifuPosition, Move => KifuMove, Transition => KifuTransition}
import jp.sndyuk.shogi.kifu.CSAExporter.TempCore.{FU,KA,HI,KI,OU,GI,KE,KY,TO,UM,RY,NG,NK,NY} // Pieces
import jp.sndyuk.shogi.kifu.CSAExporter.TempCore.{SENTE, GOTE} // Players

// Import Parsers
import jp.sndyuk.shogi.kifu.CSAParser
import jp.sndyuk.shogi.kifu.KI2Parser
// Assuming parsers return some structured representation, e.g., a list of moves or a game state.
// The exact return type will be handled based on parser API. For now, just checking successful parse.

class KifuExporterSpec extends AnyFlatSpec with Matchers {

  // --- Sample Kifu Data using TempCore types ---
  val initialKifuBoard: KifuBoard = KifuBoard(Map.empty, SENTE) // Standard Hirate assumed by exporters

  val sampleMoves1: Seq[KifuTransition] = Seq(
    KifuTransition(KifuMove(SENTE, Some(KifuPosition(7,7)), KifuPosition(7,6), FU)), // ▲7六歩  (CSA: +7776FU)
    KifuTransition(KifuMove(GOTE,  Some(KifuPosition(3,3)), KifuPosition(3,4), FU)), // △3四歩  (CSA: -3334FU)
    KifuTransition(KifuMove(SENTE, Some(KifuPosition(2,2)), KifuPosition(7,7), KA, promote = true)) // ▲7七角成 (CSA: +2277UM)
  )

  val sampleMovesWithDrop: Seq[KifuTransition] = Seq(
    KifuTransition(KifuMove(SENTE, Some(KifuPosition(7,7)), KifuPosition(7,6), FU)),
    KifuTransition(KifuMove(GOTE,  None, KifuPosition(5,5), KI, isDrop = true)) // △5五金打 (CSA: -0055KI)
  )

  // --- CSAExporter Tests ---
  "CSAExporter" should "export a simple game to CSA format" in {
    val csaOutput = CSAExporter.exportToString(initialKifuBoard, sampleMoves1, SENTE, None)

    csaOutput should startWith("V2.2\n")
    csaOutput should include ("N+SENTE_PLAYER_NAME") // Default names in TempCore based CSAExporter might differ or be absent
    csaOutput should include ("N-GOTE_PLAYER_NAME") // Default names in TempCore based CSAExporter might differ or be absent
    csaOutput should include ("+\n") // Sente moves first from initial position
    csaOutput should include ("+7776FU\n")
    csaOutput should include ("-3334FU\n")
    csaOutput should include ("+2277UM\n") // KA promoted to UM

    // Try parsing
    val parseResult = CSAParser.parse(csaOutput.linesIterator)
    parseResult.successful shouldBe true
    // Example check on parsed content:
    val parsedKifu = parseResult.get
    parsedKifu.moves.length shouldBe sampleMoves1.length
    // Note: KifuStatement inside parsedKifu.moves needs to be cast to Move to check details like piece, from, to.
    // This requires Kifu and Move case class definitions to be imported or known.
  }

  it should "export a game with drops to CSA format" in {
    val csaOutput = CSAExporter.exportToString(initialKifuBoard, sampleMovesWithDrop, GOTE, None)
    csaOutput should include ("+7776FU\n")
    csaOutput should include ("-0055KI\n") // Drop move

    val parseResult = CSAParser.parse(csaOutput.linesIterator)
    parseResult.successful shouldBe true
    parseResult.get.moves.length shouldBe sampleMovesWithDrop.length
  }

  it should "export a game ending in resignation to CSA format" in {
    val csaOutput = CSAExporter.exportToString(initialKifuBoard, sampleMoves1, SENTE, Some("%TORYO"))
    csaOutput should include ("%TORYO\n")

    val parseResult = CSAParser.parse(csaOutput.linesIterator)
    parseResult.successful shouldBe true
    // Check if the special move %TORYO is part of the parsed moves
    parsedResult.get.moves.exists {
      case SpMove(value) => value == "TORYO"
      case _ => false
    } shouldBe true
  }

  // --- KI2Exporter Tests ---
  "KI2Exporter" should "export a simple game to KI2 format" in {
    val ki2Output = KI2Exporter.exportToString(initialKifuBoard, sampleMoves1, SENTE, None) // Assuming SENTE made last move, GOTE to play

    ki2Output should include ("手合割：平手\n")
    ki2Output should include ("手数----指手----")
    ki2Output should include ("1 ▲７六歩\n")
    ki2Output should include ("2 △３四歩\n")
    ki2Output should include ("3 ▲７七角成\n")

    val parseResult = KI2Parser.parse(ki2Output.linesIterator)
    parseResult.successful shouldBe true
    // Assuming KI2Parser's Kifu structure also has a 'moves' field of appropriate type.
    // The KI2Parser internally translates KI2 moves to core.Move and updates a board state.
    // The returned 'Kifu.moves' from KI2Parser might be List[jp.sndyuk.shogi.kifu.Move] (which wraps core.Move)
    // or directly List[jp.sndyuk.shogi.core.Transition] if the parser fully processes them.
    // For now, checking length is a good first step.
    // The Kifu object from KI2Parser.parse seems to be List[KifuStatement], where a KifuStatement can be a kifu.Move.
    val parsedKifu = parseResult.get
    parsedKifu.moves.collect { case m: jp.sndyuk.shogi.kifu.Move => m }.length shouldBe sampleMoves1.length
  }

  it should "export a game with drops to KI2 format" in {
    val ki2Output = KI2Exporter.exportToString(initialKifuBoard, sampleMovesWithDrop, SENTE, None)
    ki2Output should include ("1 ▲７六歩\n")
    ki2Output should include ("2 △５五金打\n") // Drop move

    val parseResult = KI2Parser.parse(ki2Output.linesIterator)
    parseResult.successful shouldBe true
    parsedResult.get.moves.collect { case m: jp.sndyuk.shogi.kifu.Move => m }.length shouldBe sampleMovesWithDrop.length
  }

  it should "export a game with 'dou' (same position) moves to KI2 format" in {
    val movesDou = Seq(
      KifuTransition(KifuMove(SENTE, Some(KifuPosition(2,8)), KifuPosition(2,2), KA)), // ▲2二角 (moves to 2,2)
      KifuTransition(KifuMove(GOTE,  Some(KifuPosition(3,1)), KifuPosition(2,2), GI))  // △同銀 (captures on 2,2)
    )
    val ki2Output = KI2Exporter.exportToString(initialKifuBoard, movesDou, SENTE, None)
    ki2Output should include ("1 ▲２二角\n")
    // KI2Exporter's logic for '不成' might add it if piece could promote but didn't.
    // The TempCore.Move currently only has 'promote' flag, not 'didNotPromote'.
    // So, "不成" won't appear unless TempCore.Move is enhanced or KI2Exporter has specific logic.
    ki2Output should include ("2 △同　銀\n")

    val parseResult = KI2Parser.parse(ki2Output.linesIterator)
    parseResult.successful shouldBe true
    // Further validation of "同" would require inspecting the parsed move's 'fromPos' or similar.
    // The KI2Parser resolves "同" to actual coordinates during parsing.
  }

  it should "export a game ending in resignation to KI2 format" in {
    val ki2Resignation = "まで3手で先手の勝ち" // Example result string
    val ki2Output = KI2Exporter.exportToString(initialKifuBoard, sampleMoves1, SENTE, Some(ki2Resignation))
    ki2Output should include (ki2Resignation + "\n")

    val parseResult = KI2Parser.parse(ki2Output.linesIterator)
    parseResult.successful shouldBe true
    // Check if the winner information is captured by the parser, if its Kifu model supports it
    parseResult.get.winner shouldBe Some(jp.sndyuk.shogi.core.PlayerA)
  }
}

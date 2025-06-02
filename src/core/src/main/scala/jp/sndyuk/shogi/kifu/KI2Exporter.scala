package jp.sndyuk.shogi.kifu

// Removed all direct imports from jp.sndyuk.shogi.core as TempCore should provide all necessary types,
// either directly or through aliasing.
// import jp.sndyuk.shogi.core.Board.Board
// import jp.sndyuk.shogi.core.Piece.Piece
// import jp.sndyuk.shogi.core.Player.Player
// import jp.sndyuk.shogi.core.Position.Position
// import jp.sndyuk.shogi.core.Move.Move
// import jp.sndyuk.shogi.core.Transition

// Using TempCore from the kifu package
// import jp.sndyuk.shogi.kifu.TempCore // Redundant as TempCore is in the same package
import jp.sndyuk.shogi.kifu.TempCore._ // Import all members of TempCore (FU, SENTE, Position, etc.)
// Aliased imports for clarity or if there are specific needs for these names.
import jp.sndyuk.shogi.kifu.TempCore.{Move => CoreMove, Transition => CoreTransition, Board => CoreBoard, Turn => CoreTurn, Player => CorePlayer, Piece => CorePiece}


object KI2Exporter {
  // Now TempCore members like FU, SENTE, Position can be accessed directly.
  // Aliased types like CoreMove, CorePiece are also available.

  // --- KI2 Specific Mappings ---
  private def toFullWidth(n: Int): String = n.toString.map {
    // Explicitly provide promoted = false where it was intended.
    // The call sites need to be checked. For now, removing default.
    case '1' => '１'
    case '2' => '２'
    case '3' => '３'
    case '4' => '４'
    case '5' => '５'
    case '6' => '６'
    case '7' => '７'
    case '8' => '８'
    case '9' => '９'
    case '0' => '０'
    case c => c
  }.mkString

  private def toKanjiDigit(n: Int): String = n match {
    case 1 => "一"
    case 2 => "二"
    case 3 => "三"
    case 4 => "四"
    case 5 => "五"
    case 6 => "六"
    case 7 => "七"
    case 8 => "八"
    case 9 => "九"
    case _ => n.toString // Should not happen for y-coordinates
  }

  // pieceToKI2 should map the piece to its KI2 representation.
  // The 'promoted' boolean parameter was causing "馬成" instead of "角成".
  // The decision to append "成" is handled by actionStr based on move.promote.
  private def pieceToKI2(piece: CorePiece): String = piece match {
    case FU => "歩"
    case KY => "香"
    case KE => "桂"
    case GI => "銀"
    case KI => "金"
    case KA => "角"
    case HI => "飛"
    case OU => "玉" // Can also be 王 for Sente, but 玉 is common for both
    case TO => "と"
    case NY => "杏"
    case NK => "圭"
    case NG => "全"
    case UM => "馬"
    case RY => "龍"
    case _ => "?" // Should ideally not happen with TempCore definitions
  }

  private def playerToKI2(player: CorePlayer): String = player match {
    case SENTE => "▲"
    case GOTE => "△"
  }

  // --- exportToString Method ---
  def exportToString(
      board: CoreBoard, // Current or initial board state (less emphasis in KI2 vs CSA for initial state)
      history: Seq[CoreTransition], // Sequence of moves made
      currentTurnPlayer: CoreTurn, // Player whose turn it is (relevant for result string)
      result: Option[String] // Game result string, e.g., "まで77手で先手の勝ち"
  ): String = {
    val sb = new StringBuilder

    // KI2 Headers (Optional, basic examples)
    // sb.append("棋戦：(event name)\n")
    // sb.append("場所：(site)\n")
    // sb.append("持ち時間：各25分（切れたら秒読み）\n") // Time control example
    // sb.append("先手：(Sente Player Name)\n")
    // sb.append("後手：(Gote Player Name)\n")
    // sb.append("戦型：(opening name)\n") // Opening name
    sb.append("手合割：平手\n") // Board setup: Hirate (standard)
    sb.append("手数----指手---------消費時間--\n")

    var lastToPos: Option[Position] = None
    var moveNumber = 1

    history.foreach { coreTransition =>
      val move = coreTransition.move
      val playerStr = playerToKI2(move.player)

      val posStr: String = {
        if (lastToPos.contains(move.to)) {
          "同　" // Using full-width space for alignment
        } else {
          s"${toFullWidth(move.to.x)}${toKanjiDigit(move.to.y)}" // Use Kanji for Y coordinate
        }
      }

      val pieceStr = pieceToKI2(move.piece) // Pass only the piece; move.promote is for actionStr

      val actionStr = if (move.isDrop) "打" else if (move.promote) "成" else ""

      // Example line: 1 ▲７六歩 ( 0:01/0:00:01)
      // For simplicity, time consumption is omitted for now.
      // Format: moveNumber playerStr posStr pieceStr actionStr
      // Adjusted move number formatting to match test expectation (single space after number)
      sb.append(s"${moveNumber} ${playerStr}${posStr}${pieceStr}${actionStr}\n")

      coreTransition.comment.foreach { c =>
        sb.append(s"* ${c.replace("\n", "\n* ")}\n") // Comments start with *
      }

      lastToPos = Some(move.to)
      moveNumber += 1
    }

    // Game Result
    // KI2 result often includes total moves and winner, e.g.:
    // "まで108手で後手の勝ち"
    // "千日手"
    // "持将棋"
    result.foreach { resStr =>
      // This assumes resStr is already correctly formatted for KI2
      sb.append(resStr).append("\n")
    }

    sb.toString()
  }

  // Helper for testing
  def main(args: Array[String]): Unit = {
    val initialBoard = CoreBoard(Map.empty, SENTE) // Simplified

    val gameHistory = Seq(
      CoreTransition(CoreMove(SENTE, Some(Position(7,7)), Position(7,6), FU)), // ▲７六歩
      CoreTransition(CoreMove(GOTE, Some(Position(3,3)), Position(3,4), FU)), // △３四歩
      CoreTransition(CoreMove(SENTE, Some(Position(2,2)), Position(7,7), KA, promote = true)), // ▲７七角成 (assuming 7,7 was lastToPos for '同' test, but it's not here)
      CoreTransition(CoreMove(GOTE, Some(Position(7,7)), Position(7,7), GI)),      // △同　銀 (testing "同")
      CoreTransition(CoreMove(SENTE, None, Position(5,5), KI, isDrop = true))  // ▲５五金打
    )

    val turn = GOTE
    val gameResult = s"まで${gameHistory.size}手で先手の勝ち" // Example result

    println("--- KI2 Output ---")
    val ki2Output = exportToString(initialBoard, gameHistory, turn, Some(gameResult))
    println(ki2Output)
  }
}

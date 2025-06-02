package jp.sndyuk.shogi.kifu

import scala.util.parsing.combinator.RegexParsers
import jp.sndyuk.shogi.player.Utils
import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.Piece
import jp.sndyuk.shogi.core.PlayerA
import jp.sndyuk.shogi.core.PlayerB
import jp.sndyuk.shogi.core.Point
import jp.sndyuk.shogi.core.State
import jp.sndyuk.shogi.core.Turn // Turn is an alias for core.Player
import jp.sndyuk.shogi.core.PlayerA // Needed for the winner parser
import jp.sndyuk.shogi.core.PlayerB // Needed for the winner parser


object KI2Parser extends App {
  def parse(lines: Iterator[String]): KI2Parser#ParseResult[Kifu] = {
    val parser = new KI2Parser
    parser.parse(lines)
  }
}

class KI2Parser(board: Board = Board()) extends RegexParsers {
  override def skipWhitespace = false

  private var s = State()

  private val statementSep = "\n" // Changed from "," to "\n"
  private val char = s"[^$statementSep]" // Will now be [^\n]

  private val eoi = """\z""".r
  private def sep: Parser[String] = statementSep | eoi // Separator is newline or end of input

  // --- 棋譜情報
  private def kifDataFactors: Parser[List[KifuStatement]] = rep((
    // 開始日時, 終了日時, 表題, 棋戦, 戦型, 持ち時間, 場所, 掲載, ...etc
    playerAName |||
    playerBName |||
    kifuDataFactor) <~ sep)

  private def playerAName: Parser[PlayerAName] = ("先手：" ~> s"$char+".r) ^^ PlayerAName
  private def playerBName: Parser[PlayerBName] = ("後手：" ~> s"$char+".r) ^^ PlayerBName
  // Make key regex for kifuDataFactor not match "先手" or "後手" to avoid conflict
  private def kifuDataFactorKeyRegex: Parser[String] = """(?!先手|後手)[^\n：]+""".r
  private def kifuDataFactor: Parser[KifuDataFactor] = (kifuDataFactorKeyRegex ~ "：" ~ s"$char+".r) ^^ { // Reverted to $char+
    case k ~ _ ~ v => KifuDataFactor(k, v)
  }

  // --- 指し手
  // Modify move to consume optional move number, spaces, then the actual move starting with player symbol
  private def move: Parser[Move] =
    ("""[0-9]+\s*""".r).? ~ // Consume optional move number and trailing spaces
    ("▲" | "△") ~ ("１" | "２" | "３" | "４" | "５" | "６" | "７" | "８" | "９" | "同") ~ ("一" | "二" | "三" | "四" | "五" | "六" | "七" | "八" | "九").? ~
      "　".? ~ ("玉" | "歩" | "金" | "銀" | "飛" | "角" | "桂" | "香" | "と" | "成銀" | "龍" | "馬" | "成桂" | "成香") ~
      ("右" | "左" | "直" | "寄" | "引" | "打" | "上").? ~ ("右" | "左" | "直" | "引" | "寄" | "上").? ~ ("成" | "不成").? ~ ("    " | "  ").? <~ sep ^^ {
        case _ ~ p ~ x ~ yOpt ~ _ ~ pieceStr ~ detailOpt1 ~ detailOpt2 ~ nariOpt ~ _ => { // Added _ to consume the optional move number part
          val turn = if (p == "▲") PlayerA else PlayerB
          val newPos = if (x == "同") {
            s.history.head.newPos
          } else
            Board.humanReadableToPoint(x match {
              case "１" => 1
              case "２" => 2
              case "３" => 3
              case "４" => 4
              case "５" => 5
              case "６" => 6
              case "７" => 7
              case "８" => 8
              case "９" => 9
            }, yOpt match {
              case Some("一") => 1
              case Some("二") => 2
              case Some("三") => 3
              case Some("四") => 4
              case Some("五") => 5
              case Some("六") => 6
              case Some("七") => 7
              case Some("八") => 8
              case Some("九") => 9
              case _ => throw new UnsupportedOperationException
            })

          val pieceStr2 = if (x == "同" && yOpt.isDefined) {
            yOpt.get
          } else pieceStr

          val piece = pieceStr2 match {
            case "玉" => Piece.convert(Piece.◯.OU, turn)
            case "歩" => Piece.convert(Piece.◯.FU, turn)
            case "金" => Piece.convert(Piece.◯.KI, turn)
            case "銀" => Piece.convert(Piece.◯.GI, turn)
            case "飛" => Piece.convert(Piece.◯.HI, turn)
            case "角" => Piece.convert(Piece.◯.KA, turn)
            case "桂" => Piece.convert(Piece.◯.KE, turn)
            case "香" => Piece.convert(Piece.◯.KY, turn)
            case "と" => Piece.promote(Piece.convert(Piece.◯.FU, turn))
            case "成銀" => Piece.promote(Piece.convert(Piece.◯.GI, turn))
            case "龍" => Piece.promote(Piece.convert(Piece.◯.HI, turn))
            case "馬" => Piece.promote(Piece.convert(Piece.◯.KA, turn))
            case "成桂" => Piece.promote(Piece.convert(Piece.◯.KE, turn))
            case "成香" => Piece.promote(Piece.convert(Piece.◯.KY, turn))
          }

          val nari = nariOpt.exists(_ == "成")

          // Use this.board (the parser's current board state) for plans and piece checks
          val plan = Utils.plans(this.board, s).toList
          val candidates = plan.filter(t => t.newPos == newPos && this.board.piece(t.oldPos, turn) == piece).toList
          val oldPos = if (candidates.length > 1) {
            val right = detailOpt1.exists(_ == "右") || detailOpt2.exists(_ == "右")
            val left = detailOpt1.exists(_ == "左") || detailOpt2.exists(_ == "左")
            val up = detailOpt1.exists(_ == "上") || detailOpt2.exists(_ == "上")
            val near = detailOpt1.exists(_ == "直") || detailOpt2.exists(_ == "直")
            val down = detailOpt1.exists(_ == "引") || detailOpt2.exists(_ == "引")
            val side = detailOpt1.exists(_ == "寄") || detailOpt2.exists(_ == "寄")
            val captured = detailOpt1.exists(_ == "打") || detailOpt2.exists(_ == "打")
            candidates.foldLeft[Option[Point]](None) { (currOpt, transition) =>
              val point = transition.oldPos
              if (currOpt.isEmpty) {
                Some(point)
              } else {
                val curr = currOpt.get
                if (Point.isCaptured(curr) || Point.isCaptured(point)) {
                  if (captured) {
                    if (Point.isCaptured(point)) Some(point) else currOpt
                  } else if (Point.isCaptured(curr)) {
                    Some(point)
                  } else currOpt
                } else {
                  if (right) {
                    if ((turn == PlayerA && point.x > curr.x) || (turn == PlayerB && point.x < curr.x)) {
                      Some(point)
                    } else currOpt
                  } else if (left) {
                    if ((turn == PlayerA && point.x < curr.x) || (turn == PlayerB && point.x > curr.x)) {
                      Some(point)
                    } else currOpt
                  } else if (up) {
                    if ((turn == PlayerA && point.y > curr.y) || (turn == PlayerB && point.y < curr.y)) {
                      Some(point)
                    } else currOpt
                  } else if (near) {
                    if (point.x == newPos.x) {
                      Some(point)
                    } else currOpt
                  } else if (down) {
                    if ((turn == PlayerA && point.y < curr.y) || (turn == PlayerB && point.y > curr.y)) {
                      Some(point)
                    } else currOpt
                  } else if (side) {
                    if (point.y == newPos.y) {
                      Some(point)
                    } else currOpt
                  } else if (nari && transition.nari) {
                    Some(point)
                  } else currOpt
                }
              }
            }.get
          } else {
            // Ensure candidates is not empty before calling .head
            if (candidates.isEmpty) {
              // This indicates an issue: no move found for the piece to the target square.
              // Could be due to an illegal move in the KIF, or a bug in plans/piece identification.
              // For now, to avoid crash and highlight, let's throw a specific error or use a dummy oldPos.
              // However, for parsing, we must determine the oldPos or fail.
              // If it's a drop (candidates might be filtered differently), oldPos should be Point.CAPTURED or similar.
              // The current structure assumes a piece on board if not a drop.
              // Let's check if it's a drop based on details.
              val isDrop = detailOpt1.exists(_ == "打") || detailOpt2.exists(_ == "打")
              if (isDrop) {
                Point.ofCaptured(Piece.generalize(piece)) // Use generalized piece for Point.ofCaptured
              } else {
                // No candidates found for a non-drop move. This is an error in KIF or parser logic.
                throw new IllegalStateException(s"No candidate moves found for $pieceStr to $newPos for $turn. Parsed details: ${detailOpt1}, ${detailOpt2}")
              }
            } else {
              candidates.head.oldPos
            }
          }

          // Use this.board (the parser's board instance) to call the move method
          s = this.board.move(s, oldPos, newPos, true, nari)
          Move(turn, oldPos, newPos, if (nari) Piece.promote(piece) else piece, None)
        }
      }

  private def moves: Parser[List[Move]] = rep(move)

  private def comment: Parser[Comment] = (s"\\*$char*".r <~ sep) ^^ Comment

  private def winner: Parser[Turn] = s"まで[0-9]+手で".r ~ ("先手" | "後手") ~ "の勝ち" <~ sep ^^ {
    case _ ~ p ~ _ =>
      if (p == "先手") PlayerA else PlayerB // PlayerA and PlayerB are core.Player (Turn)
  }

  private def columnHeaderLine: Parser[String] = "手数----指手---------消費時間--" <~ sep

  private def statement: Parser[Kifu] = kifDataFactors ~ rep(comment).? ~ columnHeaderLine.? ~ moves ~ rep(comment).? ~ winner.? ^^ {
    case factors ~ comments1Opt ~ _ /* colHeaderOpt */ ~ mv ~ comments2Opt ~ winnerOpt =>
      // Kifu class from kifu/package.scala:
      // Kifu(version: Option[Version], kifuData: List[KifuStatement], startState: StartState, moves: List[KifuStatement], winner: Option[Turn])
      // version: Not parsed by this specific grammar part, pass None.
      // kifuData: `factors` is List[KifuDataFactor]. KifuDataFactor is a KifuStatement. This is compatible.
      // startState: Needs a StartState object. Defaulting to a simple one.
      // moves: `mv` is List[Move]. `comments` are List[Comment]. Both are KifuStatement. Concatenate them.
      // winner: `winnerOpt` is Option[Turn] (Option[core.Player]), which matches the Kifu case class.

      val allMoveStatements: List[KifuStatement] = comments1Opt.getOrElse(Nil) ++ mv ++ comments2Opt.getOrElse(Nil)
      // TODO: Parse actual StartState if available in KI2 format. For now, using a default.
      val defaultStartState = StartState(None, None, None, "+")

      Kifu(None, factors, defaultStartState, allMoveStatements, winnerOpt)
  }
  private def kifu: Parser[Kifu] = statement

  def parse(lines: Iterator[String]): ParseResult[Kifu] = {
    parseAll(kifu, lines.mkString("\n")) // Join lines with newline
  }

  // Helper for direct testing of a line
  def testSimpleLineParse(input: String): ParseResult[String] = {
    // This parser tries to match "先手：" followed by some characters, then a newline
    val lineParser = ("先手：" ~> s"$char+".r) <~ sep
    parseAll(lineParser, input)
  }

  // Helper for direct testing of a move line
  def testSimpleMoveParse(input: String): ParseResult[Move] = {
    s = State() // Reset state for parsing this move from initial board
    // The `move` parser uses `s` (State) and `board` (Board) which are class members.
    // Ensure `board` is in a state consistent with the move being parsed if needed (e.g. for `Utils.plans`).
    // For a simple first move from initial position, default Board() and fresh State() is fine.
    parseAll(move, input)
  }
}

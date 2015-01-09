package jp.sndyuk.shogi.kifu

import scala.annotation.migration
import scala.util.parsing.combinator.RegexParsers
import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.Piece
import jp.sndyuk.shogi.core.PlayerA
import jp.sndyuk.shogi.core.PlayerB
import jp.sndyuk.shogi.core.Point
import jp.sndyuk.shogi.core.State
import scala.io.Source
import jp.sndyuk.shogi.core.Rule
import jp.sndyuk.shogi.ai.simulator.Utils
import jp.sndyuk.shogi.core.Transition

object KI2Parser extends App {
  def parse(lines: Iterator[String]) = {
    val parser = new KI2Parser
    parser.parse(lines)
  }

  parse(Source.fromFile("龍王戦2014-27_1 藤井深浦.ki2", "windows-31j").getLines)
}

class KI2Parser(board: Board = Board()) extends RegexParsers {

  private var s = State()

  private val statementSep = ","
  private val char = s"[^$statementSep]"

  private val eoi = """\z""".r
  private def sep: Parser[String] = statementSep | eoi

  private def version: Parser[Version] = s"V$char+".r <~ sep ^^ Version

  // --- 棋譜情報
  private def kifDataFactors: Parser[List[KifuStatement]] = rep((
    // 開始日時, 終了日時, 表題, 棋戦, 戦型, 持ち時間, 場所, 掲載, ...etc
    playerAName |||
    playerBName |||
    kifuDataFactor) <~ sep)

  private def playerAName: Parser[PlayerAName] = ("先手：" ~> s"$char+".r) ^^ PlayerAName
  private def playerBName: Parser[PlayerBName] = ("後手：" ~> s"$char+".r) ^^ PlayerBName
  private def kifuDataFactor: Parser[KifuDataFactor] = (s"[^$statementSep：]+".r ~ "：" ~ s"$char+".r) ^^ {
    case k ~ _ ~ v => KifuDataFactor(k, v)
  }

  // --- 指し手
  private def move: Parser[Move] =
    ("▲" | "△") ~ ("１" | "２" | "３" | "４" | "５" | "６" | "７" | "８" | "９" | "同") ~ ("一" | "二" | "三" | "四" | "五" | "六" | "七" | "八" | "九").? ~
      "　".? ~ ("玉" | "歩" | "金" | "銀" | "飛" | "角" | "桂" | "香" | "と" | "成銀" | "龍" | "馬" | "成桂" | "成香") ~
      ("右" | "左" | "直" | "寄" | "引" | "打").? ~ ("右" | "左" | "直" | "引" | "寄").? ~ ("成" | "不成").? ~ ("    " | "  ").? ~ sep.? ^^ {
        case p ~ x ~ yOpt ~ _ ~ pieceStr ~ detailOpt1 ~ detailOpt2 ~ nariOpt ~ _ ~ _ => {
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
            case "玉" => Piece.invert(Piece.◯.OU, turn)
            case "歩" => Piece.invert(Piece.◯.FU, turn)
            case "金" => Piece.invert(Piece.◯.KI, turn)
            case "銀" => Piece.invert(Piece.◯.GI, turn)
            case "飛" => Piece.invert(Piece.◯.HI, turn)
            case "角" => Piece.invert(Piece.◯.KA, turn)
            case "桂" => Piece.invert(Piece.◯.KE, turn)
            case "香" => Piece.invert(Piece.◯.KY, turn)
            case "と" => Piece.toBePromoted(Piece.invert(Piece.◯.FU, turn))
            case "成銀" => Piece.toBePromoted(Piece.invert(Piece.◯.GI, turn))
            case "龍" => Piece.toBePromoted(Piece.invert(Piece.◯.HI, turn))
            case "馬" => Piece.toBePromoted(Piece.invert(Piece.◯.KA, turn))
            case "成桂" => Piece.toBePromoted(Piece.invert(Piece.◯.KE, turn))
            case "成香" => Piece.toBePromoted(Piece.invert(Piece.◯.KY, turn))
          }

          val nari = nariOpt.exists(_ == "成")
          val plan = Utils.plans(board, s, false).toList
          val candidates = plan.filter(t => t.newPos == newPos && board.piece(t.oldPos, turn) == piece).toList
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
                  if (right && ((turn == PlayerA && point.x > curr.x) || (turn == PlayerB && point.x < curr.x))) {
                    Some(point)
                  } else if (left && ((turn == PlayerA && point.x < curr.x) || (turn == PlayerB && point.x > curr.x))) {
                    Some(point)
                  } else if (up && ((turn == PlayerA && point.y > curr.y) || (turn == PlayerB && point.y < curr.y))) {
                    Some(point)
                  } else if (near && point.y == newPos.y) {
                    Some(point)
                  } else if (down && ((turn == PlayerA && point.y < curr.y) || (turn == PlayerB && point.y > curr.y))) {
                    Some(point)
                  } else if (side && point.x == newPos.x) {
                    Some(point)
                  } else if (nari && transition.nari) {
                    Some(point)
                  } else currOpt
                }
              }
            }.get
          } else candidates.head.oldPos

          s = board.move(s, oldPos, newPos, true, nari)
          Move(turn, oldPos, newPos, piece, None)
        }
      }

  private def moves: Parser[Moves] = rep(move) ^^ Moves

  // 特殊な指し手
  private def specialMove: Parser[KifuStatement] = "%" ~> s"$char+".r <~ sep ^^ SpMove

  private def statement: Parser[Kifu] = kifDataFactors ~ sep ~ moves.? ^^ {
    case kifDataFactors ~ _ ~ moves => Kifu(None, kifDataFactors, StartState(None, None, None, "+"), moves)

  }
  private def kifu: Parser[Kifu] = statement

  def parse(lines: Iterator[String]): ParseResult[Kifu] = {
    val a = lines.mkString(",")
    println(a)
    parseAll(kifu, a)
  }
}

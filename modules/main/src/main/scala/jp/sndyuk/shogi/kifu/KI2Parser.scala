package jp.sndyuk.shogi.kifu

import scala.util.parsing.combinator.RegexParsers
import jp.sndyuk.shogi.ai.simulator.Utils
import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.Piece
import jp.sndyuk.shogi.core.PlayerA
import jp.sndyuk.shogi.core.PlayerB
import jp.sndyuk.shogi.core.Point
import jp.sndyuk.shogi.core.State
import jp.sndyuk.shogi.core.Turn
import jp.sndyuk.shogi.core.PlayerA

object KI2Parser extends App {
  def parse(lines: Iterator[String]): KI2Parser#ParseResult[Kifu] = {
    val parser = new KI2Parser
    parser.parse(lines)
  }
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
      ("右" | "左" | "直" | "寄" | "引" | "打" | "上").? ~ ("右" | "左" | "直" | "引" | "寄" | "上").? ~ ("成" | "不成").? ~ ("    " | "  ").? ~ sep.? ^^ {
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
          val plan = Utils.plans(board, s).toList
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
            candidates.head.oldPos
          }

          s = board.move(s, oldPos, newPos, true, nari)
          Move(turn, oldPos, newPos, if (nari) Piece.toBePromoted(piece) else piece, None)
        }
      }

  private def moves: Parser[List[Move]] = rep(move)

  private def comment: Parser[String] = s"\\*$char*".r <~ sep

  private def winner: Parser[Turn] = s"まで[0-9]+手で".r ~ ("先手" | "後手") ~ "の勝ち" ^^ {
    case _ ~ p ~ _ =>
      if (p == "先手") PlayerA else PlayerB
  }

  private def statement: Parser[Kifu] = kifDataFactors ~ sep ~ rep(comment).? ~ moves ~ rep(comment).? ~ winner ^^ {
    case kifDataFactors ~ _ ~ _ ~ moves ~ _ ~ winner => Kifu(None, kifDataFactors, StartState(None, None, None, "+"), moves, winner)

  }
  private def kifu: Parser[Kifu] = statement

  def parse(lines: Iterator[String]): ParseResult[Kifu] = {
    parseAll(kifu, lines.mkString(","))
  }
}

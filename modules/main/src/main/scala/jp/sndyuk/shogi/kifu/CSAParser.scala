package jp.sndyuk.shogi.kifu

import util.parsing.combinator._
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import jp.sndyuk.shogi.core.PlayerA
import jp.sndyuk.shogi.core.PlayerB
import jp.sndyuk.shogi.core.Point
import jp.sndyuk.shogi.core.Board
import jp.sndyuk.shogi.core.Piece

object CSAParser extends RegexParsers {

  private val statementSep = ","
  private val char = s"[^$statementSep]"

  private val eoi = """\z""".r
  private def sep: Parser[String] = statementSep | eoi

  private def version: Parser[Version] = comment.? ~> s"V$char+".r <~ sep ^^ Version

  // --- 棋譜情報
  private def kifDataFactors: Parser[List[KifuStatement]] = rep(comment.? ~> (
    playerAName |||
    playerBName |||
    kifuDataFactor) <~ sep)

  private def playerAName: Parser[PlayerAName] = ("N+" ~> s"$char+".r) ^^ PlayerAName
  private def playerBName: Parser[PlayerBName] = ("N-" ~> s"$char+".r) ^^ PlayerBName
  private def kifuDataFactor: Parser[KifuDataFactor] = ("$" ~> s"[^$statementSep:]+".r ~ ":" ~ s"$char+".r) ^^ {
    case k ~ _ ~ v => KifuDataFactor(k, v)
  }

  // --- 開始局面
  private def startState: Parser[StartState] = (startStateOfBoard ~ first) ^^ { (details) =>
    details._1 match {
      case v: PI => StartState(Option(v.asInstanceOf[PI]), None, None, details._2)
      case v: PN => StartState(None, Option(v.asInstanceOf[PN]), None, details._2)
      case v: PP => StartState(None, None, Option(v.asInstanceOf[PP]), details._2)
    }
  }

  private def startStateOfBoard: Parser[KifuStatement] = comment.? ~> (pI | pN | pP)

  // 平手(と駒落ち)表現
  private def pI: Parser[PI] = s"PI$char+".r <~ sep ^^ PI

  // 一括表現
  private def pN: Parser[PN] = rep(s"P[1-9]$char+".r <~ sep) ^^ PN

  // 駒別単独表現
  private def pP: Parser[PP] = rep(s"P[-|+]$char+".r <~ sep) ^^ PP

  // 手番(先手)
  private def first: Parser[String] = comment.? ~> ("+" | "-") <~ sep

  // --- 指し手と消費時間
  private def move: Parser[KifuStatement] = comment.? ~> (transition | specialMove)

  private def transition: Parser[Move] =
    ("-" | "+") ~ "[1-9]".r ~ "[1-9]".r ~ "[1-9]".r ~ "[1-9]".r ~ s"$char{2}".r ~ sep ~ elapsed.? ^^ {
      case p ~ s1 ~ s2 ~ s3 ~ s4 ~ s5 ~ _ ~ elaplsed => {
        val turn = if (p == "+") PlayerA else PlayerB
        val piece = s5 match {
          case "OU" => Piece.invert(Piece.◯.OU, turn)
          case "FU" => Piece.invert(Piece.◯.FU, turn)
          case "KI" => Piece.invert(Piece.◯.KI, turn)
          case "GI" => Piece.invert(Piece.◯.GI, turn)
          case "HI" => Piece.invert(Piece.◯.HI, turn)
          case "KA" => Piece.invert(Piece.◯.KA, turn)
          case "KE" => Piece.invert(Piece.◯.KE, turn)
          case "KY" => Piece.invert(Piece.◯.KY, turn)
          case "TO" => Piece.toBePromoted(Piece.invert(Piece.◯.FU, turn))
          case "NG" => Piece.toBePromoted(Piece.invert(Piece.◯.GI, turn))
          case "RY" => Piece.toBePromoted(Piece.invert(Piece.◯.HI, turn))
          case "UM" => Piece.toBePromoted(Piece.invert(Piece.◯.KA, turn))
          case "NK" => Piece.toBePromoted(Piece.invert(Piece.◯.KE, turn))
          case "NY" => Piece.toBePromoted(Piece.invert(Piece.◯.KY, turn))
        }
        Move(turn,
          Board.humanReadableToPoint(s1.toInt, s2.toInt),
          Board.humanReadableToPoint(s3.toInt, s4.toInt),
          piece, elaplsed)
      }
    }

  private def moves: Parser[List[KifuStatement]] = rep(move)

  // 特殊な指し手
  private def specialMove: Parser[KifuStatement] = "%" ~> s"$char+".r <~ sep ^^ SpMove

  // 1手の消費時間
  private def elapsed: Parser[Elapsed] = comment.? ~> "T" ~> "[0-9]+".r <~ sep ^^ { (timeStr) => Elapsed(timeStr.toInt) }

  // コメント
  private def comment: Parser[List[Comment]] = rep(s"'$char*".r <~ sep ^^ Comment)

  private def statement: Parser[Kifu] = comment.? ~> version.? ~ kifDataFactors ~ startState ~ moves <~ comment.? ^^ {
    case version ~ kifDataFactors ~ startState ~ moves => Kifu(version, kifDataFactors, startState, moves, moves.collectFirst { case Move(p, _, _, _, _) => p }.get)

  }
  private def kifu: Parser[Kifu] = statement

  def parse(lines: Iterator[String]): ParseResult[Kifu] = {
    parseAll(kifu, lines.mkString(","))
  }
}
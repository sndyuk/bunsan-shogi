package jp.sndyuk.shogi.kifu

import util.parsing.combinator._

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

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
    ("-" | "+") ~ s"[1-9]{4}$char{2}".r ~ sep ~ elapsed.? ^^ {
      case p ~ s ~ _ ~ elaplsed => Move(p, s.substring(0, 2), s.substring(2, 4), s.substring(4, 6), elaplsed)
    }

  private def moves: Parser[Moves] = rep(move) ^^ Moves

  // 特殊な指し手
  private def specialMove: Parser[KifuStatement] = "%" ~> s"$char+".r <~ sep ^^ SpMove

  // 1手の消費時間
  private def elapsed: Parser[Elapsed] = comment.? ~> "T" ~> "[0-9]+".r <~ sep ^^ { (timeStr) => Elapsed(timeStr.toInt) }

  // コメント
  private def comment: Parser[List[Comment]] = rep(s"'$char*".r <~ sep ^^ Comment)

  private def statement: Parser[Kifu] = comment.? ~> version.? ~ kifDataFactors ~ startState ~ moves.? <~ comment.? ^^ {
    case version ~ kifDataFactors ~ startState ~ moves => Kifu(version, kifDataFactors, startState, moves)

  }
  private def kifu: Parser[Kifu] = statement

  def parseCSA(lines: Iterator[String]): ParseResult[Kifu] = {
    parseAll(kifu, lines.mkString(","))
  }
}
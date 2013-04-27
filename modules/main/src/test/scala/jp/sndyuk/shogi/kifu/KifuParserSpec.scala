package jp.sndyuk.shogi.kifu

import org.scalatest._
import scala.util.parsing.input.PagedSeqReader
import scala.collection.immutable.PagedSeq
import scala.io.Source
import scala.util.parsing.input.CharSequenceReader

class KifuParserSpec extends FlatSpec with Matchers {

  "A CSAParser" should "parse the simple kifu" in {

    val lines = """
      |'バージョン
      |V2.2
      |'対局者名
      |N+NAKAHARA
      |N-YONENAGA
      |'棋譜情報
      |'棋戦名
      |$EVENT:13th World Computer Shogi Championship
      |'対局場所
      |$SITE:KAZUSA ARC
      |'開始日時
      |$START_TIME:2003/05/03 10:30:00
      |'終了日時
      |$END_TIME:2003/05/03 11:11:05
      |'持ち時間:25分、切れ負け
      |$TIME_LIMIT:00:25+00
      |'戦型:矢倉
      |$OPENING:YAGURA
      |$UNKNOWN:HOGE
      |'平手の局面
      |P1-KY-KE-GI-KI-OU-KI-GI-KE-KY
      |P2 * -HI *  *  *  *  * -KA * 
      |P3-FU-FU-FU-FU-FU-FU-FU-FU-FU
      |P4 *  *  *  *  *  *  *  *  * 
      |P5 *  *  *  *  *  *  *  *  * 
      |P6 *  *  *  *  *  *  *  *  * 
      |P7+FU+FU+FU+FU+FU+FU+FU+FU+FU
      |P8 * +KA *  *  *  *  * +HI * 
      |P9+KY+KE+GI+KI+OU+KI+GI+KE+KY
      |'先手番
      |+
      |'指し手と消費時間
      |+2726FU
      |T12
      |%UNKNOWN
      |-3334FU
      |T6
      |%CHUDAN
      """.stripMargin.split("\n").tail.iterator

    val parseResult = CSAParser.parseCSA(lines)

    parseResult.isEmpty should be(false)
    val kifu = parseResult.get

    kifu.version should be(Option(Version("V2.2")))

    kifu.kifuData should contain(PlayerAName("NAKAHARA"))
    kifu.kifuData should contain(PlayerBName("YONENAGA"))
    kifu.kifuData should contain(KifuDataFactor("EVENT", "13th World Computer Shogi Championship"))
    kifu.kifuData should contain(KifuDataFactor("SITE", "KAZUSA ARC"))
    kifu.kifuData should contain(KifuDataFactor("START_TIME", "2003/05/03 10:30:00"))
    kifu.kifuData should contain(KifuDataFactor("END_TIME", "2003/05/03 11:11:05"))
    kifu.kifuData should contain(KifuDataFactor("TIME_LIMIT", "00:25+00"))
    kifu.kifuData should contain(KifuDataFactor("OPENING", "YAGURA"))
    kifu.kifuData should contain(KifuDataFactor("UNKNOWN", "HOGE"))

    kifu.startState should be(StartState(None, Option(PN(List(
      "P1-KY-KE-GI-KI-OU-KI-GI-KE-KY",
      "P2 * -HI *  *  *  *  * -KA * ",
      "P3-FU-FU-FU-FU-FU-FU-FU-FU-FU",
      "P4 *  *  *  *  *  *  *  *  * ",
      "P5 *  *  *  *  *  *  *  *  * ",
      "P6 *  *  *  *  *  *  *  *  * ",
      "P7+FU+FU+FU+FU+FU+FU+FU+FU+FU",
      "P8 * +KA *  *  *  *  * +HI * ",
      "P9+KY+KE+GI+KI+OU+KI+GI+KE+KY"))), None, "+"))

    kifu.moves should be(Option(Moves(List(
      Move("+", "27", "26", "FU", Option(Elapsed(12))),
      SpMove("UNKNOWN"),
      Move("-", "33", "34", "FU", Option(Elapsed(6))),
      SpMove("CHUDAN")
    ))))
  }
}
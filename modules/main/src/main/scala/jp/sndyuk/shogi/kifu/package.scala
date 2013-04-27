package jp.sndyuk.shogi

package kifu {

  trait KifuStatement

  case class Kifu(version: Option[Version], kifuData: List[KifuStatement], startState: StartState, moves: Option[Moves]) extends KifuStatement

  case class Move(player: String, oldPos: String, newPos: String, piece: String, elapsed: Option[Elapsed]) extends KifuStatement
  case class SpMove(command: String) extends KifuStatement
  case class Elapsed(sec: Int) extends KifuStatement

  case class Moves(moves: List[KifuStatement]) extends KifuStatement
  case class Version(no: String) extends KifuStatement
  case class Comment(comment: String) extends KifuStatement

  case class PlayerAName(name: String) extends KifuStatement
  case class PlayerBName(name: String) extends KifuStatement
  case class KifuDataFactor(key: String, name: String) extends KifuStatement

  case class StartState(
    pI: Option[PI] = None,
    pN: Option[PN] = None,
    pP: Option[PP] = None,
    first: String) extends KifuStatement

  case class PI(value: String) extends KifuStatement
  case class PN(value: List[String]) extends KifuStatement
  case class PP(value: List[String]) extends KifuStatement
}

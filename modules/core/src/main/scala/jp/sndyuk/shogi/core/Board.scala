package jp.sndyuk.shogi.core

import Piece._

case class Block(point: Point, piece: Piece)

case object Point {
  def ofCaptured(piece: Piece): Point = {
    piece match {
      case ◯.OU => (9, 8)
      case ◯.KI => (9, 1)
      case ◯.FU => (9, 2)
      case ◯.GI => (9, 3)
      case ◯.HI => (9, 4)
      case ◯.KA => (9, 5)
      case ◯.KE => (9, 6)
      case ◯.KY => (9, 7)
    }
  }
  @inline def isCaptured(point: Point): Boolean = point.y == 9

  def toKansuji(x: Int): String = {
    x match {
      case 0 => "一"
      case 1 => "二"
      case 2 => "三"
      case 3 => "四"
      case 4 => "五"
      case 5 => "六"
      case 6 => "七"
      case 7 => "八"
      case 8 => "九"
    }
  }
}

case class Point(y: Int, x: Int) extends Serializable {

  override def toString(): String = {
    if (Point.isCaptured(this))
      s"持駒: ${
        x match {
          case 0 => "玉"
          case 1 => "金"
          case 2 => "歩"
          case 3 => "銀"
          case 4 => "飛"
          case 5 => "角"
          case 6 => "桂"
          case 7 => "香"
        }
      }"
    else
      s"${9 - x}${Point.toKansuji(y)}"
  }
}

case object Squares {
  val allPoints: List[Point] = (for {
    y <- 0 to 8
    x <- 0 to 8
  } yield Point(y, x)).toList
}

// 0           1           2           3           4           5               
// 01234 56789 01234 56789 01234 56789 01234 56789 01234 56789 01234 56789 0123
// 00000 00000 00000 00000 00000 00000 00000 00000 00000 00000 00000 00000 ----
// 00000 00000 00000 00000 00000 00000 00000 00000 00000 00000 00000 00000 ----
// 00000 00000 00000 00000 00000 00000 00000 00000 00000 00000 00000 00000 ----
private[core] case class Squares(private[core] val bits: BitSet = BitSet(9 * 9 * BitSet.span)) {
  import Squares._

  @inline private def posOfBits(p: Point): Int = {
    val s = (p.y * 9) + p.x
    (s * BitSet.span) + ((s / 12) * 4)
  }

  def <+(piece: Piece, p: Point): Squares = {
    // assert(p.y < 9)
    // assert(p.x < 9)
    //println(s"r: ${p.rowIndex}, c: ${p.columnIndex}, maxPieceSize: $maxPieceSize, piece: ${pieceIndex(piece)}")
    set(piece, p)
    this
  }

  def id(): String = bits.id()

  def get(p: Point): Piece = {
    bits.intValue(posOfBits(p))
  }

  def setAndGet(piece: Piece, p: Point): Piece = {
    bits.replaceIntValue(piece, posOfBits(p))
  }

  // (0, 0) (0, 1) … (y, x)
  // 00000  00000
  private def set(piece: Int, p: Point): Unit = {
    bits.setInt(piece, posOfBits(p))
  }

  // (rowIndex, columnIndex)
  def find(piece: Piece): Option[Point] = allPoints.find(get(_) == piece)

  def allPieces(turn: Turn): List[Block] = allPoints.foldLeft(List[Block]()) { (xs, point) =>
    val piece = get(point)
    if (▲△(piece, turn)) Block(point, piece) :: xs else xs
  }

  private class EmptyPointIterator() extends Iterator[Point] {
    private var rest = allPoints
    private var nextPoint: Point = _
    private var full = false
    def next: Point = {
      if (hasNext) {
        full = false
        nextPoint
      } else throw new NoSuchElementException
    }
    def hasNext: Boolean = if (full) {
      true
    } else {
      full = false
      rest match {
        case Nil => false
        case x :: xs =>
          val piece = get(x)
          rest = xs
          if (piece == ❏) {
            nextPoint = x
            full = true
            true
          } else hasNext
      }
    }
  }
  def allEmptyPoints(): Iterator[Point] = new EmptyPointIterator()

  def copy(): Squares = {
    new Squares(bits.copy())
  }

  override def toString(): String = bits.toString
}

case class CapturedPieces(private[core] var playerA: Int = 0, private[core] var playerB: Int = 0) {
  // OU(2bit): 00
  // FU(6bit): 000000
  // KI(3bit): 000
  // GI(3bit): 000
  // HI(2bit): 000
  // KA(2bit): 00
  // KE(3bit): 000
  // KY(3bit): 000
  // 計 24bit
  def longValue: Long = playerA.toLong << 32 | playerB

  def id(): String = {
    java.lang.Long.toString(((playerA.toLong << 24) + playerB.toLong), 36)
  }

  def copy(): CapturedPieces = {
    new CapturedPieces(playerA + 0, playerB + 0)
  }

  def put(piece: Piece): Unit = piece match {
    case ❏ =>
    case _ if ▲(piece) =>
      // 先手側の駒なので後手の手駒に加える
      playerB = add(playerB, generalize(piece))
    case _ =>
      playerA = add(playerA, generalize(piece))
  }

  private def add(captured: Int, generalized: Piece, amount: Int = 1): Int = {
    val pos = find(generalized)
    if (pos._1 == 0) captured // ❏は無視
    else {
      updateBits(captured, count(captured, pos) + amount, pos)
    }
  }

  def get(pos: Point, turn: Turn): Option[Piece] = {
    val piece = pointToPiece(pos, turn)
    val captured = if (▲(piece)) playerA else playerB
    val gpiece = generalize(piece)
    val amount = count(captured, find(gpiece))
    if (amount > 0) {
      Some(piece)
    } else None
  }

  // 1つ減らす
  def remove(pos: Point, turn: Turn): Option[Piece] = {
    val piece = pointToPiece(pos, turn)
    val player = if (▲(piece)) playerA else playerB
    val gpiece = generalize(piece)
    if (gpiece == 32) {
      println(piece)
    }
    val amount = count(player, find(gpiece))
    if (amount > 0) {
      val updated = add(player, gpiece, -1)
      if (▲(piece)) playerA = updated else playerB = updated
      Some(piece)
    } else None
  }

  private def updateBits(captured: Int, num: Int, pos: (Int, Int)): Int = {
    val left = ~0 << (32 - pos._1)
    val right = (1 << 32 - (pos._1 + pos._2)) - 1
    val mask = left | right
    val cleared = captured & mask
    val shifted = num << (32 - (pos._1 + pos._2))

    cleared | shifted
  }

  def find(gpiece: Piece) = gpiece match {
    case ❏ => (0, 1)
    // 011
    case ◯.OU => (1, 2)
    // 000111111
    case ◯.FU => (3, 6)
    // 000000000111
    case ◯.KI => (9, 3)
    // 000000000000111
    case ◯.GI => (12, 3)
    // 00000000000000011
    case ◯.HI => (15, 2)
    // 0000000000000000011
    case ◯.KA => (17, 2)
    // 0000000000000000000111
    case ◯.KE => (19, 3)
    // 0000000000000000000000111
    case ◯.KY => (22, 3)
  }

  def pointToPiece(pos: Point, turn: Turn): Piece = {
    convert(pos.x, turn)
  }

  def count(turn: Turn, piece: Piece): Int = count(if (turn == PlayerA) playerA else playerB, find(piece))

  def count(captured: Int, pos: (Int, Int)): Int = (captured >>> (32 - (pos._1 + pos._2))) & ((1 << pos._2) - 1)

  def allPieceKinds(turn: Turn): List[Block] = {
    val player = if (turn == PlayerA) playerA else playerB
    if (player == 0) {
      Nil
    } else {
      ◯.all.foldLeft(List[Block]()) { (xs, x) =>
        if (count(player, find(x)) > 0)
          Block(Point.ofCaptured(x), convert(x, turn)) :: xs else xs
      }
    }
  }

  override def toString(): String = {
    val s = new StringBuilder
    s.append("▽ 持駒[")
    s.append("玉").append(count(playerB, find(◯.OU))).append(", ")
    s.append("歩").append(count(playerB, find(◯.FU))).append(", ")
    s.append("金").append(count(playerB, find(◯.KI))).append(", ")
    s.append("銀").append(count(playerB, find(◯.GI))).append(", ")
    s.append("飛").append(count(playerB, find(◯.HI))).append(", ")
    s.append("角").append(count(playerB, find(◯.KA))).append(", ")
    s.append("桂").append(count(playerB, find(◯.KE))).append(", ")
    s.append("香").append(count(playerB, find(◯.KY))).append("]")
    s.append(System.lineSeparator)
    s.append("△ 持駒[")
    s.append("玉").append(count(playerA, find(◯.OU))).append(", ")
    s.append("歩").append(count(playerA, find(◯.FU))).append(", ")
    s.append("金").append(count(playerA, find(◯.KI))).append(", ")
    s.append("銀").append(count(playerA, find(◯.GI))).append(", ")
    s.append("飛").append(count(playerA, find(◯.HI))).append(", ")
    s.append("角").append(count(playerA, find(◯.KA))).append(", ")
    s.append("桂").append(count(playerA, find(◯.KE))).append(", ")
    s.append("香").append(count(playerA, find(◯.KY))).append("]")
    s.toString
  }
}

object Board {

  private val table = Seq(
    Seq(△.KY, △.KE, △.GI, △.KI, △.OU, △.KI, △.GI, △.KE, △.KY),
    Seq(❏, △.HI, ❏, ❏, ❏, ❏, ❏, △.KA, ❏),
    Seq(△.FU, △.FU, △.FU, △.FU, △.FU, △.FU, △.FU, △.FU, △.FU),
    Seq(❏, ❏, ❏, ❏, ❏, ❏, ❏, ❏, ❏),
    Seq(❏, ❏, ❏, ❏, ❏, ❏, ❏, ❏, ❏),
    Seq(❏, ❏, ❏, ❏, ❏, ❏, ❏, ❏, ❏),
    Seq(▲.FU, ▲.FU, ▲.FU, ▲.FU, ▲.FU, ▲.FU, ▲.FU, ▲.FU, ▲.FU),
    Seq(❏, ▲.KA, ❏, ❏, ❏, ❏, ❏, ▲.HI, ❏),
    Seq(▲.KY, ▲.KE, ▲.GI, ▲.KI, ▲.OU, ▲.KI, ▲.GI, ▲.KE, ▲.KY))

  def apply(): Board = {
    val board = new Board()
    board.init()
    board
  }
  def apply(state: State): Board = {

    val board = Board()
    // 最新状態まで駒を進める
    state.history.reverse.foldLeft(if (state.history.size % 2 == 0) state.turn else state.turn.change) { (turn, transition) =>
      board.move(transition.oldPos, transition.newPos, turn, true)
      turn.change
    }
    board
  }

  def humanReadableToPoint(suji: Int, dan: Int): Point = dan match {
    case 0 => (9, suji)
    case _ => (dan - 1, 9 - suji)
  }
}

case class Board(squares: Squares = Squares(), val capturedPieces: CapturedPieces = CapturedPieces(), var freeze: Boolean = false) {
  def init(): Unit = {
    init2(Board.table)
  }

  // 将棋盤に駒を並べる
  def init2(pieces: Seq[Seq[Piece]], captured: Seq[(Piece, Int)] = Nil): Unit = {
    pieces.zipWithIndex.foreach { case (row, y) => row.zipWithIndex.foreach { case (piece, x) => squares <+ (piece, (y, x)) } }
    captured.foreach { case (p, c) => for (i <- 1 to c) capturedPieces.put(p) }
  }

  lazy val id: ID = {
    freeze = true
    ID(this)
  }

  def allBlocks: Seq[Block] = {
    for {
      y <- 0 to 8
      x <- 0 to 8
      p = (y, x)
    } yield Block(p, squares.get(p))
  }

  override def toString(): String = {
    val sb = new StringBuilder
    sb ++= "　9　　8　　7　　6　　5　　4　　3　　2　　1"
    sb ++= System.lineSeparator
    val s = allBlocks.foldLeft(sb) { (s, block) =>
      s ++= pieceString(block.piece)
      s ++= "|"
      if (block.point.x == 8) {
        s ++= Point.toKansuji(block.point.y)
        s ++= System.lineSeparator
        s ++= "ーーーーーーーーーーーーーーーーーーーーーーーー"
        s ++= System.lineSeparator
      }
      s
    }
    s ++= capturedPieces.toString
    s.toString
  }

  def move(state: State, oldPos: Point, newPos: Point, validation: Boolean, nari: Boolean): State = {
    if (freeze) throw new IllegalStateException
    val pieceOldPos = pieceOnBoardNotEmpty(newPos)
    if (!move(oldPos, newPos, state.turn, validation, nari)) {
      throw new IllegalStateException(s"Cound not move $oldPos to $newPos, Turn: ${state.turn}")
    }
    State(Transition(oldPos, newPos, nari, pieceOldPos) :: state.history, state.turn.change)
  }

  def moveOpt(state: State, oldPos: Point, newPos: Point, validation: Boolean, nari: Boolean): Option[State] = {
    if (freeze) throw new IllegalStateException
    val pieceOldPos = pieceOnBoardNotEmpty(newPos)
    if (move(oldPos, newPos, state.turn, validation, nari)) {
      Some(State(Transition(oldPos, newPos, nari, pieceOldPos) :: state.history, state.turn.change))
    } else {
      None
    }
  }

  def piece(pos: Point, turn: Turn): Piece = if (isCaptured(pos))
    capturedPieces.get(pos, turn).get
  else squares.get(pos)

  def pieceOnBoard(pos: Point): Option[Piece] = if (isCaptured(pos)) None else Option(squares.get(pos))
  def pieceOnBoardNotEmpty(pos: Point): Option[Piece] = pieceOnBoard(pos).collect { case p if p != ❏ => p }

  // Returns true: moved, false: can't move
  private def move(oldPos: Point, newPos: Point, turn: Turn, validation: Boolean, nari: Boolean = false): Boolean = {
    if (freeze) throw new IllegalStateException
    val p = piece(oldPos, turn)
    if (!validation || Rule.canMove(this, p, oldPos, newPos, turn, nari)) {
      if (Point.isCaptured(oldPos)) {
        capturedPieces.remove(oldPos, turn)
        squares <+ (p, newPos)
      } else {
        val oldPiece = squares.setAndGet(❏, oldPos)
        capturedPieces.put(squares.setAndGet(if (nari) promote(oldPiece) else oldPiece, newPos))
      }
      true
    } else false
  }

  private def moveRollback(transition: Transition, turn: Turn): Unit = {
    if (freeze) throw new IllegalStateException
    if (Point.isCaptured(transition.oldPos)) {
      capturedPieces.put(capturedPieces.pointToPiece(transition.oldPos, turn.change))
      squares.setAndGet(❏, transition.newPos)
    } else {
      val p = piece(transition.newPos, turn)
      squares.setAndGet(transition.captured.getOrElse(❏), transition.newPos)
      squares.setAndGet(if (transition.nari) reverseIfPromoted(p) else p, transition.oldPos)
      transition.captured.foreach(p => capturedPieces.remove(Point.ofCaptured(generalize(p)), turn))
    }
  }

  def allMovablePieces(turn: Turn): List[Block] = {
    capturedPieces.allPieceKinds(turn) ::: squares.allPieces(turn)
  }

  def allEmptyPoints(): Iterator[Point] = squares.allEmptyPoints()
  def isFinish(turn: Turn): Boolean = capturedPieces.count(turn, ◯.OU) != 0
  def isFinish(): Boolean = isFinish(PlayerA) || isFinish(PlayerB)
  def isCaptured(pos: Point) = Point.isCaptured(pos)
  def copy(): Board = {
    new Board(squares.copy(), capturedPieces.copy())
  }
  def newBoard(state: State): Board = {
    val board = Board()
    state.history.reverse.foldLeft[Turn](PlayerA) { (turn, next) =>
      board.move(next.oldPos, next.newPos, turn, true, next.nari)
      turn.change
    }
    board
  }

  def rollback(state: State): State = {
    if (freeze) throw new IllegalStateException
    moveRollback(state.history.head, state.turn.change)
    state.previous
  }

  def getString(p: Point): String = pieceString(squares.get(p))
}

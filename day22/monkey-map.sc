val input = io.Source.fromFile("input.txt").getLines.toVector

val width = input.dropRight(2).map(_.length).max
val height = input.length - 2
val rows = input.dropRight(2).map(_.toCharArray().padTo(width, ' ').toVector)
val cols = rows.transpose

enum Turn:
  case None, Right, Unused, Left

case class Move(count: Int, turn: Turn)

def getMoves(move_codes: String): List[Move] =
  if move_codes.isEmpty then Nil
  else
    val (num, rest) = move_codes.span(_.isDigit)
    val turn = rest match
      case ""                 => Turn.None
      case x if x.head == 'L' => Turn.Left
      case x if x.head == 'R' => Turn.Right
    Move(num.toInt, turn) :: getMoves(rest.tail)

val moves = getMoves(input.last)

enum Heading:
  case Right, Down, Left, Up

case class Position(x: Int, y: Int, f: Heading)
case class Wall(x: Int, y: Int)
case class PositionCube(w: Wall, p: Position)

def turn(pos: PositionCube, turn: Turn): PositionCube =
  val nextHeading = Heading.fromOrdinal((pos.p.f.ordinal + turn.ordinal) % 4)
  pos.copy(p = pos.p.copy(f = nextHeading))

def symbolAt(pos: PositionCube): Char =
  val (x, y) = (pos.w.x * size, pos.w.y * size)
  rows(y + pos.p.y)(x + pos.p.x)

def execute(next: PositionCube => PositionCube)(from: PositionCube, move: Move): PositionCube =
  if move.count == 0 then turn(from, move.turn)
  else
    val nxt = next(from)
    if symbolAt(nxt) == '#' then turn(from, move.turn)
    else execute(next)(nxt, Move(move.count - 1, move.turn))

def next1(from: PositionCube): PositionCube =
  def next(from: Position): Position =
    val next = from.f match
      case Heading.Right => from.copy(x = (from.x + 1) % width)
      case Heading.Down  => from.copy(y = (from.y + 1) % height)
      case Heading.Left  => from.copy(x = (from.x + width - 1) % width)
      case Heading.Up    => from.copy(y = (from.y + height - 1) % height)
    rows(next.y)(next.x) match
      case ' ' =>
        next.f match
          case Heading.Right => next.copy(x = rows(next.y).indexWhere(_ != ' '))
          case Heading.Down  => next.copy(y = cols(next.x).indexWhere(_ != ' '))
          case Heading.Left  => next.copy(x = rows(next.y).lastIndexWhere(_ != ' '))
          case Heading.Up    => next.copy(y = cols(next.x).lastIndexWhere(_ != ' '))
      case _ => next

  from.copy(p = next(from.p))

def encode(pos: PositionCube): Int =
  val (wx, wy) = (pos.w.x * size, pos.w.y * size)
  val pos2D = Position(wx + pos.p.x, wy + pos.p.y, pos.p.f)
  ((pos2D.y + 1) * 1000) + ((pos2D.x + 1) * 4) + pos2D.f.ordinal

val start = Position(rows(0).indexOf('.'), 0, Heading.Right)
val start1 = PositionCube(Wall(0, 0), start)
val end1 = moves.foldLeft(start1)(execute(next1))
println(encode(end1))

// part 2
val size = 50

// hardcoded for input.txt
// 012
//0 ##
//1 #
//2##
//3#
val walls = Map(
  Wall(1, 0) -> Map(
    Heading.Right -> Position(2, 0, Heading.Right),
    Heading.Down -> Position(1, 1, Heading.Down),
    Heading.Left -> Position(0, 2, Heading.Right), // upside down
    Heading.Up -> Position(0, 3, Heading.Right) // transpose
  ),
  Wall(2, 0) -> Map(
    Heading.Right -> Position(1, 2, Heading.Left), // upside down
    Heading.Down -> Position(1, 1, Heading.Left), // transpose
    Heading.Left -> Position(1, 0, Heading.Left),
    Heading.Up -> Position(0, 3, Heading.Up)
  ),
  Wall(1, 1) -> Map(
    Heading.Right -> Position(2, 0, Heading.Up), // transpose
    Heading.Down -> Position(1, 2, Heading.Down),
    Heading.Left -> Position(0, 2, Heading.Down), // transpose
    Heading.Up -> Position(1, 0, Heading.Up)
  ),
  Wall(1, 2) -> Map(
    Heading.Right -> Position(2, 0, Heading.Left), // upside down
    Heading.Down -> Position(0, 3, Heading.Left), // transpose
    Heading.Left -> Position(0, 2, Heading.Left),
    Heading.Up -> Position(1, 1, Heading.Up)
  ),
  Wall(0, 2) -> Map(
    Heading.Right -> Position(1, 2, Heading.Right),
    Heading.Down -> Position(0, 3, Heading.Down),
    Heading.Left -> Position(1, 0, Heading.Right), // upside down
    Heading.Up -> Position(1, 1, Heading.Right) // transpose
  ),
  Wall(0, 3) -> Map(
    Heading.Right -> Position(1, 2, Heading.Up), // transpose
    Heading.Down -> Position(2, 0, Heading.Down),
    Heading.Left -> Position(1, 0, Heading.Down), // transpose
    Heading.Up -> Position(0, 2, Heading.Up)
  )
)

def opposite(facing: Heading): Heading =
  Heading.fromOrdinal((facing.ordinal + 2) % 4)

def edgePosition(i: Int, f: Heading): Position =
  f match
    case Heading.Right => Position(0, i, f)
    case Heading.Down  => Position(i, 0, f)
    case Heading.Left  => Position(size - 1, i, f)
    case Heading.Up    => Position(i, size - 1, f)

def next2(from: PositionCube): PositionCube =
  val facing = from.p.f
  facing match
    case Heading.Right =>
      from.p.x + 1 match
        case x if x == size =>
          val trans = walls(from.w)(facing)
          val i = if trans.f == opposite(facing) then size - 1 - from.p.y else from.p.y
          PositionCube(Wall(trans.x, trans.y), edgePosition(i, trans.f))
        case x => from.copy(p = from.p.copy(x = x))
    case Heading.Left =>
      from.p.x - 1 match
        case -1 =>
          val trans = walls(from.w)(facing)
          val i = if trans.f == opposite(facing) then size - 1 - from.p.y else from.p.y
          PositionCube(Wall(trans.x, trans.y), edgePosition(i, trans.f))
        case x => from.copy(p = from.p.copy(x = x))
    case Heading.Down =>
      from.p.y + 1 match
        case y if y == size =>
          val trans = walls(from.w)(facing)
          val i = if trans.f == opposite(facing) then size - 1 - from.p.x else from.p.x
          PositionCube(Wall(trans.x, trans.y), edgePosition(i, trans.f))
        case y => from.copy(p = from.p.copy(y = y))
    case Heading.Up =>
      from.p.y - 1 match
        case -1 =>
          val trans = walls(from.w)(facing)
          val i = if trans.f == opposite(facing) then size - 1 - from.p.x else from.p.x
          PositionCube(Wall(trans.x, trans.y), edgePosition(i, trans.f))
        case y => from.copy(p = from.p.copy(y = y))

val start2 = PositionCube(Wall(start.x / size, 0), Position(0, 0, Heading.Right))
val end2 = moves.foldLeft(start2)(execute(next2))
println(encode(end2))

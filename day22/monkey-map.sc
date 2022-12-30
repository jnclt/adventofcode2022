val input = io.Source.fromFile("input.txt").getLines.toVector

val width = input.dropRight(2).map(_.length).max
val height = input.length - 2
val rows = input.dropRight(2).map(_.toCharArray().padTo(width, ' ').toVector)
val cols = rows.transpose

enum Turn:
  case None, Right, Unused, Left

case class Move(count: Int, turn: Turn)

def get_moves(move_codes: String): List[Move] =
  if move_codes.isEmpty then Nil
  else
    val (num, rest) = move_codes.span(_.isDigit)
    val turn = rest match
      case ""                 => Turn.None
      case x if x.head == 'L' => Turn.Left
      case x if x.head == 'R' => Turn.Right
    Move(num.toInt, turn) :: get_moves(rest.tail)

val moves = get_moves(input.last)

enum Heading:
  case Right, Down, Left, Up

case class Position(x: Int, y: Int, f: Heading)

def next1(from: Position): Position =
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

def execute(next: Position => Position)(from: Position, move: Move): Position =
  val nextHeading = Heading.fromOrdinal((from.f.ordinal + move.turn.ordinal) % 4)
  if move.count == 0 then from.copy(f = nextHeading)
  else
    val nxt = next(from)
    if rows(nxt.y)(nxt.x) == '#' then from.copy(f = nextHeading)
    else execute(next)(nxt, Move(move.count - 1, move.turn))

def encode(pos: Position): Int =
  ((pos.y + 1) * 1000) + ((pos.x + 1) * 4) + pos.f.ordinal

val start = Position(rows(0).indexOf('.'), 0, Heading.Right)
val end = moves.foldLeft(start)(execute(next1))
println(encode(end))

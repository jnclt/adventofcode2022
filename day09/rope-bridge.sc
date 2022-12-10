case class Move(dir: (Int, Int), dist: Int)
def toMove(line: String): Move =
  val split = line.split(" ")
  Move(
    split(0) match
      case "R" => (1, 0)
      case "L" => (-1, 0)
      case "U" => (0, 1)
      case "D" => (0, -1)
    ,
    split(1).toInt
  )

def dist(head: (Int, Int), tail: (Int, Int)): (Int, Int) =
  ((head._1 - tail._1).abs, (head._2 - tail._2).abs)

case class Position(head: (Int, Int), tail: List[(Int, Int)])
def step(from: Position, dir: (Int, Int)): Position =
  val newHead = (from.head._1 + dir._1, from.head._2 + dir._2)
  val tail = from.tail.head
  val newTail = dist(newHead, tail) match
    case (2, _) => (tail._1 + dir._1, newHead._2) :: from.tail
    case (_, 2) => (newHead._1, tail._2 + dir._2) :: from.tail
    case _      => from.tail
  Position(newHead, newTail)

def move(from: Position, by: Move): Position =
  List.fill(by.dist)(by.dir).foldLeft(from)(step)

val moves = io.Source.fromFile("input.txt").getLines.map(toMove)
val moved = moves.foldLeft(Position((0, 0), (0, 0) :: Nil))(move)
println(moved.tail.toSet.size)

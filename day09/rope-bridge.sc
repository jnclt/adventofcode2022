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
    case (2, 2) => (tail._1 + dir._1, tail._2 + dir._2) :: from.tail
    case (2, _) => (tail._1 + dir._1, newHead._2) :: from.tail
    case (_, 2) => (newHead._1, tail._2 + dir._2) :: from.tail
    case _      => from.tail
  Position(newHead, newTail)

def move(from: Position, by: Move): Position =
  List.fill(by.dist)(by.dir).foldLeft(from)(step)

val moves = io.Source.fromFile("input.txt").getLines.map(toMove)
val start = Position((0, 0), (0, 0) :: Nil)
val moved = moves.foldLeft(start)(move)
println(moved.tail.toSet.size)

//part2
def follow(from: Position, track: List[(Int, Int)]): Position =
  val dirs =
    (from.head :: track.reverse).sliding(2).map(p => (p(1)._1 - p(0)._1, p(1)._2 - p(0)._2))
  dirs.foldLeft(from)(step)

val movedEnd = (2 to 9).foldLeft(moved)((prev, _) => follow(start, prev.tail))
println(movedEnd.tail.toSet.size)

//debug
def printPosition(p: Position): Unit =
  val positions = p.tail.toSet
  val (xs, ys) = positions.unzip
  for y <- ys.max to ys.min by -1 do
    println()
    for x <- xs.min to xs.max do print(if positions.contains(x, y) then "#" else ".")
  println()

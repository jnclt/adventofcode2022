val start = io.Source
  .fromFile("input.txt")
  .getLines
  .zipWithIndex
  .flatMap((l, y) => l.zipWithIndex.filter(_._1 == '#').map((_, x) => (x, y)))
  .toList

enum Heading:
  case North, South, West, East

def side(pos: (Int, Int), h: Heading): (Set[(Int, Int)], (Int, Int)) =
  val (x, y) = pos
  h match
    case Heading.North => ((-1 to 1).map(i => (x + i, y - 1)).toSet, (x, y - 1))
    case Heading.South => ((-1 to 1).map(i => (x + i, y + 1)).toSet, (x, y + 1))
    case Heading.West  => ((-1 to 1).map(i => (x - 1, y + i)).toSet, (x - 1, y))
    case Heading.East  => ((-1 to 1).map(i => (x + 1, y + i)).toSet, (x + 1, y))

def target(occupied: Set[(Int, Int)], offset: Int)(pos: (Int, Int)): Option[(Int, Int)] =
  val (x, y) = pos
  val allNeighbors: Set[(Int, Int)] =
    (for i <- -1 to 1; j <- -1 to 1 yield (x + i, y + j)).toSet - ((x, y))
  if (allNeighbors & occupied).isEmpty then None
  else
    val targets = (for i <- 0 to 3 yield
      val (neighbors, move) = side(pos, Heading.fromOrdinal((offset + i) % 4))
      if neighbors.intersect(occupied).isEmpty then Some(move) else None
    )
    targets.find(_.isDefined).getOrElse(None)

def turn(occupied: List[(Int, Int)], i: Int): List[(Int, Int)] =
  val targets = occupied.map(target(occupied.toSet, i % 4))
  val conflicts = targets.distinct.filter(p => targets.count(_ == p) > 1).toSet + None
  occupied.zip(targets).map((o, t) => if conflicts.contains(t) then o else t.get)

def bboxCount(occupied: List[(Int, Int)]): Int =
  val (xs, ys) = occupied.unzip
  val width = xs.max - xs.min + 1
  val height = ys.max - ys.min + 1
  width * height - occupied.size

val end = (0 to 9).foldLeft(start)(turn)
println(bboxCount(end))

var i = 0
var occupied = start
var next = turn(occupied, i)
while occupied != next do
  i += 1
  occupied = next
  next = turn(next, i)
println(i + 1)

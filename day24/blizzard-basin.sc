val lines = io.Source.fromFile("input.txt").getLines.toList
val height = lines.size - 2
val width = lines.head.size - 2

def parse(line: String): Set[(Int, Int)] =
  line
    .drop(1)
    .dropRight(1)
    .zipWithIndex
    .flatMap((c, i) =>
      c match
        case '>' => Some((i, 1))
        case '<' => Some((i, -1))
        case _   => None
    )
    .toSet

val horizontal = lines.drop(1).dropRight(1).map(parse).toVector
val vertical = lines.transpose
  .drop(1)
  .dropRight(1)
  .map(chars =>
    chars
      .map(_ match
        case 'v' => '>'
        case '^' => '<'
        case _   => '%'
      )
      .mkString
  )
  .map(parse)
  .toVector

def isFree(pos: ((Int, Int), Int)): Boolean =
  val ((x, y), time) = pos
  !(horizontal(y).contains(((x + width - (time % width)) % width, 1)) ||
    horizontal(y).contains(((x + (time % width)) % width, -1)) ||
    vertical(x).contains(((y + height - (time % height)) % height, 1)) ||
    vertical(x).contains(((y + (time % height)) % height, -1)))

def traverse(from: (Int, Int), to: (Int, Int), at: Int): Int =
  var startTime = at
  while !isFree(from, startTime) do startTime += 1
  val toVisit = collection.mutable.Queue((from, startTime))
  while !toVisit.isEmpty do
    val (pos, ts) = toVisit.dequeue()
    if pos == to then
      return ts + 1
      toVisit.clear()
    else
      val time = ts + 1
      val (x, y) = pos
      val candidates = List(
        if x < width - 1 then Some((x + 1, y), time) else None,
        if y < height - 1 then Some((x, y + 1), time) else None,
        if x > 0 then Some((x - 1, y), time) else None,
        if y > 0 then Some((x, y - 1), time) else None,
        Some((pos), time)
      ).flatten.filter(isFree(_)).filterNot(c => toVisit.contains(c))
      toVisit.enqueueAll(candidates)
  return -1

val start = (0, 0)
val end = (width - 1, height - 1)
val there = (traverse(start, end, 0))
println(there)
val back = traverse(end, start, there)
val thereAgain = (traverse(start, end, back))
println(thereAgain)

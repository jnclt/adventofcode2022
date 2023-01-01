val lines = io.Source.fromFile("input.txt").getLines.toList
val height = lines.size - 2
val width = lines.head.size - 2

case class Blizzard(idx: Byte, step: Byte):
  def idxInTime(time: Int, span: Int): Int =
    val offset = (idx + step * time) % span
    if offset < 0 then offset + span else offset

def parse(line: String): Vector[Blizzard] =
  line
    .drop(1)
    .dropRight(1)
    .zipWithIndex
    .flatMap((c, i) =>
      c match
        case '>' => Some(Blizzard(i.toByte, 1.toByte))
        case '<' => Some(Blizzard(i.toByte, -1.toByte))
        case _   => None
    )
    .toVector

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

def isFree(pos: (Int, Int), time: Int): Boolean =
  val (x, y) = pos
  horizontal(y).forall(_.idxInTime(time, width) != x) &&
  vertical(x).forall(_.idxInTime(time, height) != y)

val end = (width - 1, height - 1)

def traverse(): Unit =
  var time = 1
  val toVisit = collection.mutable.Queue(Set((0, 0)))
  while !toVisit.isEmpty do
    time += 1
    val batch = toVisit.dequeue()
    print(s"$time:${batch.size},")
    val newBatch = batch.flatMap(pos =>
      if pos == end then
        println(time)
        System.exit(0)
        toVisit.clear
        List()
      else
        val (x, y) = pos
        val candidates = List(
          if x < width - 1 then Some(x + 1, y) else None,
          if y < height - 1 then Some(x, y + 1) else None,
          if x > 0 then Some(x - 1, y) else None,
          if y > 0 then Some(x, y - 1) else None,
          Some(pos)
        ).flatten.filter(isFree(_, time))
        candidates
    )
    toVisit.enqueue(newBatch)

println(traverse())

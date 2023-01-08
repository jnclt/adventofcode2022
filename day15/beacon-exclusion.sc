val record = """^.*x=(-?\d+), y=(-?\d+): .* x=(-?\d+), y=(-?\d+)$""".r

// sensor = (x, y, range)
// beacon = (x, y)
val (sensors, beacons) = io.Source
  .fromFile("input.txt")
  .getLines
  .map(l =>
    l match
      case record(sxs, sys, bxs, bys) =>
        val (sx, sy, bx, by) = (sxs.toInt, sys.toInt, bxs.toInt, bys.toInt)
        (
          (sx, sy, (bx - sx).abs + (by - sy).abs),
          (bx, by)
        )
  )
  .toList
  .unzip

val line = 2000000

def intersection(line: Int)(sensor: (Int, Int, Int)): Range =
  val side = sensor._3 - (sensor._2 - line).abs
  (sensor._1 - side) to (sensor._1 + side)

val covered = sensors.map(intersection(line)).map(_.toSet).reduce(_ ++ _)
val beaconsInLine = beacons.filter(_._2 == line).map(_._1).toSet
println((covered &~ beaconsInLine).size)

// part2: ~4min runtime
def sortedSensors = sensors.sortWith((a, b) => a._3 > b._3)

def croppedIntersection(line: Int, limit: Int)(sensor: (Int, Int, Int)): Range =
  val range = intersection(line)(sensor)
  math.max(0, range.start) to math.min(limit, range.end)

def merge(range: Range, added: Range): Option[Range] =
  if range.end < added.start - 1 || range.start > added.end + 1 then None
  else Some(math.min(range.start, added.start) to math.max(range.end, added.end))

def addRange(ranges: Set[Range], added: Range): Set[Range] =
  if added.isEmpty then ranges
  else
    ranges.find(r => merge(r, added).isDefined) match
      case Some(r) => addRange(ranges - r, merge(r, added).get)
      case None    => ranges + added

def coverage(line: Int, limit: Int): Set[Range] =
  sortedSensors.foldLeft(Set[Range]()) { case (ranges, sensor) =>
    addRange(ranges, croppedIntersection(line, limit)(sensor))
  }

val limit = 4000000
val y = (1 to limit).find(coverage(_, limit).size > 1).get
val x = coverage(y, limit).toList.sortWith((a, b) => a.start < b.start).head.end + 1
println(x * BigInt(4000000) + y)

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

def intersection(sensor: (Int, Int, Int)): Range =
  val side = sensor._3 - (sensor._2 - line).abs
  (sensor._1 - side) to (sensor._1 + side)

val coverage = sensors.map(intersection).map(_.toSet).reduce(_ ++ _)
val beaconsInLine = beacons.filter(_._2 == line).map(_._1).toSet
println((coverage &~ beaconsInLine).size)

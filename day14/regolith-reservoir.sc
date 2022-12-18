val lines = io.Source
  .fromFile("input.txt")
  .getLines
  .map(_.split(" -> ").map(p => (p.slice(0, 3).toInt, p.slice(4, p.size).toInt)))

def polylineToPoints(corners: Array[(Int, Int)]): Set[(Int, Int)] =
  def lineToPoints(start: (Int, Int), end: (Int, Int)): Set[(Int, Int)] =
    if start._1 == end._1 then
      val step = if start._2 < end._2 then 1 else -1
      Range(start._2, end._2 + step, step).map(row => (start._1, row)).toSet
    else
      val step = if start._1 < end._1 then 1 else -1
      Range(start._1, end._1 + step, step).map(col => (col, start._2)).toSet

  corners.sliding(2).foldLeft(Set[(Int, Int)]())((p, c) => p ++ lineToPoints(c.head, c.tail.head))

val rocks = lines.flatMap(polylineToPoints).toSet
val start = (500, 0)
val bottom = rocks.map(_._2).max

def fall(pos: (Int, Int), surface: Set[(Int, Int)]): Option[(Int, Int)] =
  if pos._2 > bottom then None // abyss
  else if surface.contains(pos._1, pos._2 + 1) then
    if !surface.contains(pos._1 - 1, pos._2 + 1) then
      fall((pos._1 - 1, pos._2 + 1), surface) // left
    else if !surface.contains(pos._1 + 1, pos._2 + 1) then
      fall((pos._1 + 1, pos._2 + 1), surface) // right
    else Some(pos) // stay
  else fall((pos._1, pos._2 + 1), surface) // down

def pour(sand: Set[(Int, Int)]): Set[(Int, Int)] =
  fall(start, rocks ++ sand) match
    case None           => sand
    case Some(sandcorn) => pour(sand + sandcorn)

println(pour(Set[(Int, Int)]()).size)

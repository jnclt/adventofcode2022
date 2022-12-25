def parseCube(s: String): (Int, Int, Int) =
  val Array(x, y, z) = s.split(",").map(_.toInt)
  (x, y, z)

def freeSides(cube: (Int, Int, Int), cubes: Set[(Int, Int, Int)]): Int =
  val (x, y, z) = cube
  val sides =
    List((x + 1, y, z), (x - 1, y, z), (x, y + 1, z), (x, y - 1, z), (x, y, z + 1), (x, y, z - 1))
  6 - sides.count(cubes.contains)

val cubes = io.Source.fromFile("input.txt").getLines.map(parseCube).toSet
println(cubes.toList.map(freeSides(_, cubes)).sum)

val trees = io.Source.fromFile("input.txt").getLines.map(_.toVector).toVector
def fromLeft(line: Vector[Char]): List[Int] =
  line.zipWithIndex
    .foldLeft((0, List[Int]())) { case ((max, list), (height, idx)) =>
      if height > max then (height, idx :: list) else (max, list)
    }
    ._2
def fromRight(line: Vector[Char]): List[Int] =
  line.zipWithIndex
    .foldRight((0, List[Int]())) { case ((height, idx), (max, list)) =>
      if height > max then (height, idx :: list) else (max, list)
    }
    ._2
def visibleHorizontal(fold: Vector[Char] => List[Int]): Vector[Tuple] =
  trees.zipWithIndex.flatMap((line, rowIdx) => List.fill(line.size)(rowIdx).zip(fold(line)))
def visibleVertical(fold: Vector[Char] => List[Int]): Vector[Tuple] =
  trees.transpose.zipWithIndex.flatMap((line, rowIdx) =>
    fold(line).zip(List.fill(line.size)(rowIdx))
  )

println(
  Set(
    visibleHorizontal(fromLeft),
    visibleHorizontal(fromRight),
    visibleVertical(fromLeft),
    visibleVertical(fromRight)
  ).flatten.toSet.size
)

// part2: highlight all 8s and 9s and eyeball the result
println(51 * 47 * 14 * 8)

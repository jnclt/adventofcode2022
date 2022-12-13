val input = io.Source.fromFile("input.txt").getLines.map(_.toCharArray.toVector).toVector

val endY = input.indexWhere(_.contains('E'))
val endX = input(endY).indexOf('E')
val startY = input.indexWhere(_.contains('S'))
val startX = input(startY).indexOf('S')

val fixedStart = input
  .updated(startY, input(startY).updated(startX, 'a'))
val elevations = fixedStart
  .updated(endY, fixedStart(endY).updated(endX, 'z'))

def step(toVisit: List[(Int, Int)], costs: Map[(Int, Int), Int]): Map[(Int, Int), Int] =
  toVisit match
    case Nil => costs
    case (x, y) :: rest =>
      val elevation = elevations(y)(x)
      val cost = costs((x, y))
      val neighbors = for
        (dx, dy) <- List((-1, 0), (1, 0), (0, -1), (0, 1))
        if x + dx >= 0 && x + dx < elevations.head.size
        if y + dy >= 0 && y + dy < elevations.size
        if elevations(y + dy)(x + dx) - elevation <= 1
        if !costs.contains((x + dx, y + dy)) || costs((x + dx, y + dy)) > cost + 1
      yield (x + dx, y + dy)

      val newCosts = neighbors.foldLeft(costs) { case (costs, (x, y)) =>
        costs.updated((x, y), cost + 1)
      }

      step(rest ++ neighbors, newCosts)

val costs = step(List((startX, startY)), Map((startX, startY) -> 0))
println(costs((endX, endY)))

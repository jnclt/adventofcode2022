import collection.mutable.Stack

val input = io.Source.fromFile("input.txt").getLines
val stackInput = input.takeWhile(!_.isBlank).toList
val stackCount = stackInput.last.trim.split("\\s+").last.toInt
val initialStacks =
  stackInput.dropRight(1).foldRight(Vector.fill(stackCount)(List[Char]())) { case (line, stacks) =>
    stacks
      .zip(line.grouped(4))
      .map { case (stack, item) =>
        if item(1) == ' ' then stack else item(1) :: stack
      }
      .toVector
  }

val moves = input.toList
val move = """^move (\d+) from (\d+) to (\d+)$""".r
def rearrange(reverse: Boolean): Vector[List[Char]] =
  moves.foldLeft(initialStacks) { (stacks, line) =>
    val move(count, from, to) = line: @unchecked
    val fromIdx = from.toInt - 1
    val toIdx = to.toInt - 1
    val (items, rest) = stacks(fromIdx).splitAt(count.toInt)
    stacks
      .updated(fromIdx, rest)
      .updated(toIdx, (if reverse then items.reverse else items) ::: stacks(toIdx))
  }
println(rearrange(true).map(_.head).mkString)
println(rearrange(false).map(_.head).mkString)

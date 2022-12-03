val lines = io.Source.fromFile("input.txt").getLines().toList
val Value_a = 65
val Value_A = 97

def priority(item: Char): Int =
  val charValue = item.toInt
  charValue - (if charValue >= Value_A then Value_A - 1 else Value_a - 1 - 26)

val duplicatePriorities = lines
  .map(line =>
    line.toCharArray.grouped(line.size / 2).map(_.toSet).reduce(_ & _).map(priority).head
  )
println(duplicatePriorities.sum)

val badgePriorities = lines
  .map(_.toCharArray.toSet)
  .grouped(3)
  .map(_.reduce(_ & _).map(priority).head)
println(badgePriorities.sum)

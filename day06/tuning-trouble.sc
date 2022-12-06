println(
  io.Source
    .fromFile("input.txt")
    .getLines
    .next
    .sliding(4)
    .indexWhere(_.distinct.size == 4) + 4
)

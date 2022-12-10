def signal(ctr: Int): Boolean =
  (ctr - 20) % 40 == 0

val addx = """^addx\s(-?\d+)$""".r
val total =
  io.Source.fromFile("input.txt").getLines.foldLeft((0, 1, 0)) { case ((ctr, reg, total), line) =>
    line match
      case "noop" if signal(ctr + 1) =>
        (ctr + 1, reg, total + (reg * (ctr + 1)))
      case "noop" if !signal(ctr + 1) => (ctr + 1, reg, total)
      case addx(arg) if signal(ctr + 1) =>
        (ctr + 2, reg + arg.toInt, total + (reg * (ctr + 1)))
      case addx(arg) if signal(ctr + 2) =>
        val next = total + (reg * (ctr + 2))
        (ctr + 2, reg + arg.toInt, total + (reg * (ctr + 2)))
      case addx(arg) =>
        (ctr + 2, reg + arg.toInt, total)
  }
println(total._3)

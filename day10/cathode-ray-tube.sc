import scala.collection.immutable.Queue
val commands = io.Source.fromFile("input.txt").getLines.toList

def signal(ctr: Int): Boolean =
  (ctr - 20) % 40 == 0

val addx = """^addx\s(-?\d+)$""".r
val total = commands
  .foldLeft((0, 1, 0)) { case ((ctr, reg, total), line) =>
    line match
      case "noop" if signal(ctr + 1) =>
        (ctr + 1, reg, total + (reg * (ctr + 1)))
      case "noop" if !signal(ctr + 1)   => (ctr + 1, reg, total)
      case addx(arg) if signal(ctr + 1) => (ctr + 2, reg + arg.toInt, total + (reg * (ctr + 1)))
      case addx(arg) if signal(ctr + 2) =>
        (ctr + 2, reg + arg.toInt, total + (reg * (ctr + 2)))
      case addx(arg) =>
        (ctr + 2, reg + arg.toInt, total)
  }
  ._3
println(total)

val registers = commands
  .foldLeft(Queue(1)) { case (regs, line) =>
    line match
      case "noop"    => regs.enqueue(regs.last)
      case addx(arg) => regs.enqueue(regs.last).enqueue(regs.last + arg.toInt)
  }

val screen = registers.zipWithIndex.map { case (reg, i) =>
  val col = i % 40
  if col >= reg - 1 && col <= reg + 1 then '#'
  else '.'
}

screen.grouped(40).foreach(row => println(row.mkString))

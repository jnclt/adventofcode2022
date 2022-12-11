import collection.mutable.Queue
case class Monkey(
    items: Queue[Int],
    op: Int => Int,
    div: Int,
    next: (Int, Int),
    var counter: Int = 0
):
  def process(): Unit =
    val toInspect = items.dequeueAll(_ => true)
    toInspect.foreach { item =>
      counter += 1
      val newItem = op(item) / 3
      val recipient = (newItem % div) match
        case 0 => monkeys(next._1)
        case _ => monkeys(next._2)
      recipient.items.enqueue(newItem)
    }

def parse(lines: Iterator[String]): Monkey =
  lines.drop(1) // "Monkey X"
  val items = Queue(lines.next.split(": ").last.split(", ").map(_.toInt)*)
  val cmd = lines.next.split("= ").last.split(" ")
  val op = (old: Int) =>
    cmd(2) match
      case "old"              => old * old
      case _ if cmd(1) == "+" => old + cmd(2).toInt
      case _ if cmd(1) == "*" => old * cmd(2).toInt
  val div = lines.next.split("divisible by ").last.toInt
  val next = (lines.next.last.toString.toInt, lines.next.last.toString.toInt)
  Monkey(items, op, div, next)

def round(): Unit =
  monkeys.foreach(_.process())

val monkeys =
  io.Source.fromFile("input.txt").getLines.grouped(7).map(_.iterator).map(parse).toVector

(1 to 20).foreach(_ => round())
println(monkeys.map(_.counter).sortWith(_ > _).take(2).product)

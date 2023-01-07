import collection.mutable.Queue
case class Monkey(
    items: Queue[BigInt],
    op: BigInt => BigInt,
    div: Int,
    next: (Int, Int),
    var counter: BigInt = 0
):
  def process(control: BigInt => BigInt): Unit =
    val toInspect = items.dequeueAll(_ => true)
    toInspect.foreach { item =>
      counter += 1
      val newItem = control(op(item))
      val recipient = if newItem % div == 0 then monkeys(next._1) else monkeys(next._2)
      recipient.items.enqueue(newItem)
    }

def parse(lines: Iterator[String]): Monkey =
  lines.drop(1) // "Monkey X"
  val items = Queue(lines.next.split(": ").last.split(", ").map(i => BigInt(i))*)
  // val items = Queue(lines.next.split(": ").last.split(", ").map(_.toInt)*)
  val cmd = lines.next.split("= ").last.split(" ")
  val div = lines.next.split("divisible by ").last.toInt
  val next = (lines.next.last.toString.toInt, lines.next.last.toString.toInt)
  val op = (old: BigInt) =>
    cmd(2) match
      case "old"              => old.pow(2)
      case _ if cmd(1) == "+" => old + cmd(2).toInt
      case _ if cmd(1) == "*" => old * cmd(2).toInt
  Monkey(items, op, div, next)

def round(control: BigInt => BigInt): Unit =
  monkeys.foreach(_.process(control))

def monkeyBusiness(): BigInt =
  monkeys.map(_.counter).sortWith(_ > _).take(2).product

val input = io.Source.fromFile("input.txt").getLines.grouped(7).toList
var monkeys = input.map(_.iterator).map(parse).toList

def control1(item: BigInt): BigInt = item / 3
(1 to 20).foreach(_ => round(control1))
println(monkeyBusiness())

monkeys = input.map(_.iterator).map(parse).toList
val magic = BigInt(monkeys.map(_.div).product)
def control2(item: BigInt): BigInt = item % magic
(1 to 10000).foreach(_ => round(control2))
println(monkeyBusiness())

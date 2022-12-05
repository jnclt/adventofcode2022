import collection.mutable.Stack

val input = io.Source.fromFile("input copy.txt").getLines
val stackInput = Stack().pushAll(input.takeWhile(!_.isBlank))
val stackCount = stackInput.pop.trim.split("\\s+").last.toInt
val stacks = Vector.fill(stackCount)(Stack[Char]())
stackInput.foldLeft(stacks) { (stacks, line) =>
  stacks.zip(line.grouped(4)).foreach { case (stack, item) =>
    if item(1) != ' ' then stack.push(item(1))
  }
  stacks
}
val move = """^move (\d+) from (\d+) to (\d+)$""".r
input.foreach { case move(count, from, to) =>
  val fromStack = stacks(from.toInt - 1)
  val toStack = stacks(to.toInt - 1)
  val items = fromStack.take(count.toInt)
  fromStack.dropInPlace(count.toInt)
  toStack.pushAll(items)
}
println(stacks.map(_.top).mkString)

import collection.mutable.ListBuffer

def shift(v: ListBuffer[(BigInt, Int)])(n: (BigInt, Int)): Unit =
  val size = v.size - 1
  val i = v.indexOf(n)
  val j = ((i + n._1) % size).toInt match
    case 0 if n._1 != 0 => size
    case x if x < 0     => x + size
    case x              => x
  v.remove(i)
  v.insert(j, n)

def coordsSum(decoded: ListBuffer[(BigInt, Int)]): BigInt =
  val start = decoded.indexWhere(_._1 == 0)
  val coords = List(1000, 2000, 3000).map(i => (i + start) % decoded.size)
  coords.map(decoded(_)._1).sum

val numbers = io.Source.fromFile("input.txt").getLines.map(BigInt(_)).toList
val values = numbers.zipWithIndex
val v = ListBuffer.from(values)

values.map(shift(v))
println(coordsSum(v))

val key = 811589153
val values2 = numbers.map(_ * key).zipWithIndex
val v2 = ListBuffer.from(values2)
for _ <- 1 to 10
do values2.map(shift(v2))
println(coordsSum(v2))

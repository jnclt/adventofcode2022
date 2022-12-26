// import collection.mutable.ListBuffer

// val values = io.Source.fromFile("input2.txt").getLines.map(_.toInt).toList
// val v = ListBuffer.from(values)
// val size = v.size - 1

// def shift(n: Int): Unit =
//   val i = v.indexOf(n)
//   val j = (i + n) % size match
//     case 0 if n != 0 => size
//     case x if x < 0  => x + size
//     case x           => x
//   v.remove(i)
//   v.insert(j, n)
//   // print(".")
//   println(v.mkString(","))

// println(v.mkString(","))
// values.map(shift)
// val start = v.indexOf(0)
// println(start)
// val coords = List(1000, 2000, 3000).map(i => (i + start) % v.size)
// println(coords)
// println(coords.map(v(_)).sum)

import collection.mutable.ListBuffer

val values = io.Source.fromFile("input.txt").getLines.map(_.toInt).zipWithIndex.toList
val v = ListBuffer.from(values)
val size = v.size - 1

def shift(n: (Int, Int)): Unit =
  val i = v.indexOf(n)
  val j = (i + n._1) % size match
    case 0 if n._1 != 0 => size
    case x if x < 0     => x + size
    case x              => x
  v.remove(i)
  v.insert(j, n)

values.map(shift)
val start = v.indexWhere(_._1 == 0)
val coords = List(1000, 2000, 3000).map(i => (i + start) % v.size)
println(coords.map(v(_)._1).sum)

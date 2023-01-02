import Math.pow, Math.log

val lines = io.Source.fromFile("input.txt").getLines.map(_.toCharArray).toList
val maxWidth = lines.maxBy(_.size).size
val numbers = lines.map(_.reverse.padTo(maxWidth, '0'))

val toDigit = Map('=' -> -2, '-' -> -1, '0' -> 0, '1' -> 1, '2' -> 2)

def toDecimal(n: Array[Char]): BigInt =
  def sumUp(acc: BigInt, v: (Char, Int)): BigInt =
    val (c, i) = v
    (toDigit(c) * BigDecimal(pow(5, i)).toBigInt) + acc
  n.zipWithIndex.foldLeft(BigInt(0))(sumUp)

def baseFiveDigits(n: BigInt): List[(Int, Int)] =
  if n == 0 then Nil
  else
    val exp = (log(n.doubleValue) / log(5)).toInt
    val divisor = BigDecimal(pow(5, exp)).toBigInt
    val value = (n / divisor).toInt
    (value, exp) :: baseFiveDigits(n % divisor)

// 0 -> 0, 1 -> 1, 2 -> 2, 3 -> 1=, 4 -> 1-, 5 -> 10
val fromDigit =
  Map(0 -> ('0', 0), 1 -> ('1', 0), 2 -> ('2', 0), 3 -> ('=', 1), 4 -> ('-', 1), 5 -> ('0', 1))

def encodeBaseFive(reversed: List[Int], carryOver: Int): List[Char] =
  reversed match
    case Nil => if carryOver == 0 then Nil else carryOver.toString.head :: Nil
    case x :: xs =>
      val (c, co) = fromDigit(x + carryOver)
      c :: encodeBaseFive(xs, co)

def fromDecimal(n: BigInt): String =
  val digits = baseFiveDigits(n)
  val vec = Vector.fill(digits.head._2 + 1)(0)
  val baseFiveReversed = digits.foldLeft(vec) { case (v, p) => v.updated(p._2, p._1) }.toList
  encodeBaseFive(baseFiveReversed, 0).reverse.mkString

val decimal = numbers.map(toDecimal).toList
println(fromDecimal(decimal.sum))

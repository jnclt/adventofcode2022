import scala.io.Source

def calorieCounting(inputPath: String): Unit =
  val lines = Source.fromFile(inputPath).getLines()
  def sums(lines: Iterator[String]): List[Int] =
    if lines.isEmpty then Nil
    else
      val (prefix, suffix) = lines.span(!_.isEmpty)
      prefix.map(_.toInt).sum :: sums(suffix.dropWhile(_.isEmpty))

  println(sums(lines).max)

calorieCounting(args(0))

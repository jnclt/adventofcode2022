import scala.io.Source

def rockPaperScissors(inputPath: String): Unit =
  val wins = Map("A Y" -> 6, "B Z" -> 6, "C X" -> 6)
  val draws = Map("A X" -> 3, "B Y" -> 3, "C Z" -> 3)
  val losses = Map("A Z" -> 0, "B X" -> 0, "C Y" -> 0)
  val rules1 = (wins ++ draws ++ losses, Map('X' -> 1, 'Y' -> 2, 'Z' -> 3))

  val rocks = Map("A Y" -> 1, "B X" -> 1, "C Z" -> 1)
  val papers = Map("A Z" -> 2, "B Y" -> 2, "C X" -> 2)
  val scissors = Map("A X" -> 3, "B Z" -> 3, "C Y" -> 3)
  val rules2 = (rocks ++ papers ++ scissors, Map('X' -> 0, 'Y' -> 3, 'Z' -> 6))

  def score(rules: (Map[String, Int], Map[Char, Int]))(turn: String): Int =
    rules._1(turn) + rules._2(turn.last)

  val lines = Source.fromFile(inputPath).getLines().toList

  println(lines.map(score(rules1)).sum)
  println(lines.map(score(rules2)).sum)

rockPaperScissors(args(0))

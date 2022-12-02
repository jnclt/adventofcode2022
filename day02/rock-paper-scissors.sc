import scala.io.Source

def rockPaperScissors(inputPath: String): Unit =
  val wins = Set(
    "A Y",
    "B Z",
    "C X"
  )
  val draws = Set(
    "A X",
    "B Y",
    "C Z"
  )
  val points = Map('X' -> 1, 'Y' -> 2, 'Z' -> 3)

  def score(turn: String): Int =
    val matchScore = if draws.contains(turn) then 3 else if wins.contains(turn) then 6 else 0
    val pointScore = points(turn.charAt(2))
    matchScore + pointScore

  val total = Source.fromFile(inputPath).getLines().map(score).sum
  println(total)

rockPaperScissors(args(0))

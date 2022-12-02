import scala.io.Source

def rockPaperScissors(inputPath: String): Unit =
  val wins = Set("A Y", "B Z", "C X")
  val draws = Set("A X", "B Y", "C Z")
  val points = Map('X' -> 1, 'Y' -> 2, 'Z' -> 3)

  val matches = Map('X' -> 0, 'Y' -> 3, 'Z' -> 6)
  val rocks = Set("A Y", "B X", "C Z")
  val papers = Set("A Z", "B Y", "C X")

  def score1(turn: String): Int =
    val matchScore = if draws.contains(turn) then 3 else if wins.contains(turn) then 6 else 0
    val pointScore = points(turn.charAt(2))
    matchScore + pointScore

  def score2(turn: String): Int =
    val pointScore = if rocks.contains(turn) then 1 else if papers.contains(turn) then 2 else 3
    val matchScore = matches(turn.charAt(2))
    matchScore + pointScore

  val lines = Source.fromFile(inputPath).getLines().toList

  println(lines.map(score1).sum)
  println(lines.map(score2).sum)

rockPaperScissors(args(0))

val vars =
  io.Source.fromFile("input.txt").getLines.map(_.split(": ")).map(l => l(0) -> l(1)).toMap

def number = """(\d+)""".r
def add = """(\w+) \+ (\w+)""".r
def times = """(\w+) \* (\w+)""".r
def sub = """(\w+) - (\w+)""".r
def div = """(\w+) / (\w+)""".r

def eval(expr: String): BigInt =
  vars(expr) match
    case number(n)   => BigInt(n)
    case add(a, b)   => eval(a) + eval(b)
    case times(a, b) => eval(a) * eval(b)
    case sub(a, b)   => eval(a) - eval(b)
    case div(a, b)   => eval(a) / eval(b)

println(eval("root"))

enum Side:
  case Left, Right

def parent(expr: String): (String, Side) =
  val (k, v) = vars.find(_._2.contains(expr)).get
  (k, if v.startsWith(expr) then Side.Left else Side.Right)

def flip(tupled: (String, Side)): BigInt =
  val (expr, side) = tupled
  if expr == "root" then
    side match
      case Side.Left  => eval(vars("root").split(" ").last)
      case Side.Right => eval(vars("root").split(" ").head)
  else
    vars(expr) match
      case add(a, b) if side == Side.Left    => flip(parent(expr)) - eval(b)
      case add(a, b) if side == Side.Right   => flip(parent(expr)) - eval(a)
      case sub(a, b) if side == Side.Left    => flip(parent(expr)) + eval(b)
      case sub(a, b) if side == Side.Right   => eval(a) - flip(parent(expr))
      case times(a, b) if side == Side.Left  => flip(parent(expr)) / eval(b)
      case times(a, b) if side == Side.Right => flip(parent(expr)) / eval(a)
      case div(a, b) if side == Side.Left    => flip(parent(expr)) * eval(b)
      case div(a, b) if side == Side.Right   => eval(a) / flip(parent(expr))

println(flip(parent("humn")))

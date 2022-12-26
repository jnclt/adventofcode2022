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

val pair = """^(\d+)-(\d+),(\d+)-(\d+)""".r
case class SectionPair(ls: Int, le: Int, rs: Int, re: Int)
println(
  io.Source
    .fromFile("input.txt")
    .getLines
    .map(l =>
      l match
        case pair(ls, le, rs, re) => SectionPair(ls.toInt, le.toInt, rs.toInt, re.toInt)
    )
    .count(p => (p.ls >= p.rs && p.le <= p.re) || (p.rs >= p.ls && p.re <= p.le))
)

val pair = """(\d+)-(\d+),(\d+)-(\d+)""".r
case class SectionPair(ls: Int, le: Int, rs: Int, re: Int)
val sectionPairs = pair
  .findAllMatchIn(io.Source.fromFile("input.txt").mkString)
  .map(m => SectionPair(m.group(1).toInt, m.group(2).toInt, m.group(3).toInt, m.group(4).toInt))
  .toList
println(sectionPairs.count(p => (p.ls >= p.rs && p.le <= p.re) || (p.rs >= p.ls && p.re <= p.le)))
println(sectionPairs.count(p => (p.ls >= p.rs && p.ls <= p.re) || (p.rs >= p.ls && p.rs <= p.le)))

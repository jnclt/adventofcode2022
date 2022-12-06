val codes = io.Source.fromFile("input.txt").getLines.next
println(codes.sliding(4).indexWhere(_.distinct.size == 4) + 4)
println(codes.sliding(14).indexWhere(_.distinct.size == 14) + 14)

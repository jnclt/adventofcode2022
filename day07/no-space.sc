import scala.collection.mutable.ArrayBuffer
case class Node(name: String, weight: Option[Int], children: ArrayBuffer[Node])
val input = io.Source.fromFile("input.txt").getLines

val down = """^\$\scd\s([a-z/]+)$""".r
val up = """^\$\scd\s\.\.$""".r
val file = """^(\d+)\s(.+)$""".r

def process(input: Iterator[String], cwd: ArrayBuffer[Node]): Node =
  val line = input.nextOption
  line match
    case Some(down(name)) =>
      val node = Node(name, None, ArrayBuffer())
      cwd.head.children.prepend(node)
      cwd.prepend(node)
      process(input, cwd)
    case Some(file(weight, name)) =>
      cwd.head.children.prepend(Node(name, Some(weight.toInt), ArrayBuffer()))
      process(input, cwd)
    case Some(up()) => process(input, cwd.tail)
    case Some(_)    => process(input, cwd)
    case None       => cwd.last

val root = Node("/", None, ArrayBuffer())
input.next
process(input, ArrayBuffer(root))

def sizes(dir: Node): List[Int] =
  val subdirSizes = dir.children.filter(!_.weight.isDefined).map(sizes).toList
  val fileSizes = dir.children.filter(_.weight.isDefined).map(_.weight.get).toList
  (subdirSizes.map(_.head).sum + fileSizes.sum) :: subdirSizes.flatten

val dirSizes = sizes(root)
println(dirSizes.filter(_ <= 100000).sum)
println(dirSizes.filter(_ > (30000000 - (70000000 - dirSizes.head))).min)

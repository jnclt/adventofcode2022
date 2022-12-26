//> using lib "org.scala-lang.modules::scala-parallel-collections:1.0.4"

import java.util.Calendar
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.CollectionConverters._

case class Minerals(ore: Int, clay: Int, obsidian: Int, geodes: Int):
  def increment(robots: Minerals): Minerals =
    Minerals(
      ore + robots.ore,
      clay + robots.clay,
      obsidian + robots.obsidian,
      geodes + robots.geodes
    )
  def decrement(costs: Minerals): Minerals =
    Minerals(
      ore - costs.ore,
      clay - costs.clay,
      obsidian - costs.obsidian,
      geodes - costs.geodes
    )

case class Blueprint(
    oreR: Minerals,
    clayR: Minerals,
    obsidianR: Minerals,
    geodeR: Minerals
)

val blueprintPattern =
  """Blueprint \d+: Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.""".r
def parseBlueprint(line: String): Blueprint =
  line match
    case blueprintPattern(ore, clay, obsidianOre, obsidianClay, geodeOre, geodeObsidian) =>
      Blueprint(
        Minerals(ore.toInt, 0, 0, 0),
        Minerals(clay.toInt, 0, 0, 0),
        Minerals(obsidianOre.toInt, obsidianClay.toInt, 0, 0),
        Minerals(geodeOre.toInt, 0, geodeObsidian.toInt, 0)
      )

// return supplies + robots => only need to go down 6 more steps from there
def geodesMaxCount(steps: Int, supplies: Minerals, robots: Minerals)(
    bp: Blueprint
): (Minerals, Minerals) =
  if steps == 0 then (supplies, robots)
  else
    val counts = ArrayBuffer.empty[(Minerals, Minerals)]
    val nextSupplies = supplies.increment(robots)
    if supplies.ore >= bp.geodeR.ore && supplies.obsidian >= bp.geodeR.obsidian then
      counts.addOne(
        geodesMaxCount(
          steps - 1,
          nextSupplies.decrement(bp.geodeR),
          robots.copy(geodes = robots.geodes + 1)
        )(bp)
      )
    if supplies.ore >= bp.obsidianR.ore && supplies.clay >= bp.obsidianR.clay then
      counts.addOne(
        geodesMaxCount(
          steps - 1,
          nextSupplies.decrement(bp.obsidianR),
          robots.copy(obsidian = robots.obsidian + 1)
        )(bp)
      )
    if supplies.ore >= bp.clayR.ore then
      counts.addOne(
        geodesMaxCount(
          steps - 1,
          nextSupplies.decrement(bp.clayR),
          robots.copy(clay = robots.clay + 1)
        )(bp)
      )
    if supplies.ore >= bp.oreR.ore then
      counts.addOne(
        geodesMaxCount(
          steps - 1,
          nextSupplies.decrement(bp.oreR),
          robots.copy(ore = robots.ore + 1)
        )(bp)
      )
    counts.addOne(geodesMaxCount(steps - 1, nextSupplies, robots)(bp))
    counts.maxBy((supplies, robots) => robots.geodes)

def geodes(steps: Int, supplies: Minerals, robots: Minerals)(bp: Blueprint): (Minerals, Minerals) =
  println(s"${Calendar.getInstance().getTime()}: starting Blueprint: $bp")
  val count = geodesMaxCount(steps, supplies, robots)(bp)
  println(s"${Calendar.getInstance().getTime()}: count: $count, Blueprint: $bp")
  return count

val blueprints = io.Source.fromFile("input.txt").getLines.map(parseBlueprint).toList

// part1
val endStates = blueprints.par.map(
  geodes(24, Minerals(0, 0, 0, 0), Minerals(1, 0, 0, 0))
) // 37min with 10 threads
val geodeCounts = endStates.map(_._1.geodes)
println(geodeCounts.zipWithIndex.map(p => p._1 * (p._2 + 1)).sum)

// part2
// not-reliable, 32 steps takes too long => split into 2 phases and hope the optimal solution comes out
val endStatesPhase1 = blueprints
  .take(3)
  .par
  .map(
    // 25 turns out to produce a state from which the optimum can be reached
    geodes(25, Minerals(0, 0, 0, 0), Minerals(1, 0, 0, 0))
  ) // 36min with 3 threads

val endStatesPahse2 = blueprints.zip(endStatesPhase1).map((bp, st) => geodes(7, st._1, st._2)(bp))
println(endStatesPahse2.map(_._1.geodes).product)

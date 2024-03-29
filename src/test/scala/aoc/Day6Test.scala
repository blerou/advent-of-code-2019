package aoc

import org.scalatest.flatspec.AnyFlatSpec

class Day6Test extends AnyFlatSpec {
  import Day6._

  "The example" should "match" in {
    val rawInput = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"
    val input = parse(rawInput.split('\n').toIterable)
    val result = orbitCount(input)
    assert(result === 42)
  }

  "The YOU-SAN example" should "work" in {
    val rawInput = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"
    val input = parse(rawInput.split('\n').toIterable)
    val result = orbitalTransfers(input)
    assert(result === 4)
  }
}

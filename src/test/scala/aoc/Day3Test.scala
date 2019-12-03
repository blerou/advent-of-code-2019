package aoc

import org.scalatest.flatspec.AnyFlatSpec

class Day3Test extends AnyFlatSpec{
  import Day3._

  "The path point" should "be generated from instructions" in {
    assert(Set((5,6), (5,7)) === instPoints("U2", (5,5)))
    assert(Set((5,4), (5,3)) === instPoints("D2", (5,5)))
    assert(Set((4,5), (3,5)) === instPoints("L2", (5,5)))
    assert(Set((6,5), (7,5)) === instPoints("R2", (5,5)))

    assert(Set((6,5), (7,5), (7,6)) === instPoints("R2,U1", (5,5)))
  }

  "The example" should "match" in {
    assert(
      159 ===
      smallestDistance(
        "R75,D30,R83,U83,L12,D49,R71,U7,L72",
        "U62,R66,U55,R34,D71,R55,D58,R83"
      )
    )
  }
}

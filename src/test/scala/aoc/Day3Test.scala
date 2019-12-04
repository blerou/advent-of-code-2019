package aoc

import org.scalatest.flatspec.AnyFlatSpec

class Day3Test extends AnyFlatSpec {
  import Day3._

  "The path point" should "be generated from instructions" in {
    assert(Seq((5, 6), (5, 7)) === instPoints("U2", (5, 5)))
    assert(Seq((5, 4), (5, 3)) === instPoints("D2", (5, 5)))
    assert(Seq((4, 5), (3, 5)) === instPoints("L2", (5, 5)))
    assert(Seq((6, 5), (7, 5)) === instPoints("R2", (5, 5)))

    assert(Seq((6, 5), (7, 5), (7, 6)) === instPoints("R2,U1", (5, 5)))
  }

  "The distance example" should "match" in {
    assert(
      159 === smallestDistance(
        "R75,D30,R83,U83,L12,D49,R71,U7,L72",
        "U62,R66,U55,R34,D71,R55,D58,R83"
      )
    )
  }

  "The step example" should "match" in {
    assert(
      610 === smallestSteps(
        "R75,D30,R83,U83,L12,D49,R71,U7,L72",
        "U62,R66,U55,R34,D71,R55,D58,R83"
      )
    )
    assert(
      410 === smallestSteps(
        "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
        "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
      )
    )
  }
}

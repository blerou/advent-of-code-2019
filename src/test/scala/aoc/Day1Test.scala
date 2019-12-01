package aoc

import org.scalatest.flatspec.AnyFlatSpec

class Day1Test extends AnyFlatSpec{
  import Day1._

  "The mass" should "match the calculated value" in {
    assert(2 === fuelRequired(12))
    assert(2 === fuelRequired(14))
    assert(654 === fuelRequired(1969))
    assert(33583 === fuelRequired(100756))
  }

  "The fuel fuel meta" should "match the examples" in {
    assert(2 === fuelFuel(14))
    assert(966 === fuelFuel(1969))
    assert(50346 === fuelFuel(100756))
  }
}

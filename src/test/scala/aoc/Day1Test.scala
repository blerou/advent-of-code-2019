package aoc

import Day1.fuelRequired
import org.scalatest.flatspec.AnyFlatSpec

class Day1Test extends AnyFlatSpec{
  "The mass" should "match the calculated value" in {
    assert(2 === fuelRequired(12))
    assert(2 === fuelRequired(14))
    assert(654 === fuelRequired(1969))
    assert(33583 === fuelRequired(100756))
  }
}

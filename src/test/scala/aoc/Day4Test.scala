package aoc

import org.scalatest.flatspec.AnyFlatSpec

class Day4Test extends AnyFlatSpec {
  import Day4._

  "The password " should "contains double" in {
    assert(valid(112345))
    assert(valid(11))
    assert(valid(122))
    assert(valid(1111122))
  }

  "The password" should "invalid when no doubles" in {
    assert(!valid(12))
    assert(!valid(34567))
  }

  "The password" should "contain numbers in not decreasing order" in {
    assert(valid(11234))
    assert(!valid(332))
    assert(!valid(11243))
    assert(!valid(33214))
  }
}

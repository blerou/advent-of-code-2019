package aoc

import org.scalatest.flatspec.AnyFlatSpec

class Day4Test extends AnyFlatSpec {
  import Day4._

  "The password " should "contains double" in {
    assert(valid(112345, Seq(double)))
    assert(valid(11, Seq(double)))
    assert(valid(122, Seq(double)))
    assert(valid(2211111, Seq(double)))
  }

  "The password" should "invalid when no doubles" in {
    assert(!valid(12, Seq(double)))
    assert(!valid(34567, Seq(double)))
  }

  "The password" should "contain numbers in not decreasing order" in {
    assert(valid(1234, Seq(increase)))
    assert(!valid(32, Seq(increase)))
    assert(!valid(1243, Seq(increase)))
    assert(!valid(3214, Seq(increase)))
  }

  "The not part of larger group rule" should "apply to the examples" in {
    assert(hasPair("112233"))
    assert(!hasPair("123444"))
    assert(hasPair("111122"))
  }
}

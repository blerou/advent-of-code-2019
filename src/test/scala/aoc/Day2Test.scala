package aoc

import org.scalatest.flatspec.AnyFlatSpec

class Day2Test extends AnyFlatSpec{
  "The example input intcode" should "match the example output" in {
    assert(Array(2,0,0,0,99) === Day2.run(Array(1,0,0,0,99)))
    assert(Array(2,3,0,6,99) === Day2.run(Array(2,3,0,3,99)))
    assert(Array(2,4,4,5,99,9801) === Day2.run(Array(2,4,4,5,99,0)))
    assert(Array(30,1,1,4,2,5,6,0,99) === Day2.run(Array(1,1,1,4,99,5,6,0,99)))
  }
}

package aoc

object Day4 {

  /*

--- Day 4: Secure Container ---

You arrive at the Venus fuel depot only to discover it's protected by a password. The Elves had written the password on a sticky note, but someone threw it out.

However, they do remember a few key facts about the password:

    It is a six-digit number.
    The value is within the range given in your puzzle input.
    Two adjacent digits are the same (like 22 in 122345).
    Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).

Other than the range rule, the following are true:

    111111 meets these criteria (double 11, never decreases).
    223450 does not meet these criteria (decreasing pair of digits 50).
    123789 does not meet these criteria (no double).

How many different passwords within the range given in your puzzle input meet these criteria?

Your puzzle input is 136760-595730.

   */

  def load: (Int, Int) = (136760, 595730)

  def valid(i: Int): Boolean = {
    val s = i.toString
    lazy val double: Boolean = (0 until s.length - 1).foldLeft(false) {
      case (double, i) =>
        double || s(i) == s(i + 1)
    }
    lazy val increase: Boolean = (0 until s.length - 1).foldLeft(true) {
      case (inc, i) =>
        inc && s(i) <= s(i + 1)
    }
    double && increase
  }

  def passwordCount(low: Int, up: Int): Int = {
    var count = 0
    for (i <- low to up if valid(i))
      count += 1
    count
  }

  def part1(): Unit = {
    val (low, up) = load
    val result = passwordCount(low, up)
    println(s"part 1 solution: $result")
  }

  def main(args: Array[String]): Unit = {
    part1()
  }
}

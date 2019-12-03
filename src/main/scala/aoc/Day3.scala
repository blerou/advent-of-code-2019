package aoc

object Day3 {

  /*

--- Day 3: Crossed Wires ---

The gravity assist was successful, and you're well on your way to the Venus refuelling station. During the rush back on Earth, the fuel management system wasn't completely installed, so that's next on the priority list.

Opening the front panel reveals a jumble of wires. Specifically, two wires are connected to a central port and extend outward on a grid. You trace the path each wire takes as it leaves the central port, one wire per line of text (your puzzle input).

The wires twist and turn, but the two wires occasionally cross paths. To fix the circuit, you need to find the intersection point closest to the central port. Because the wires are on a grid, use the Manhattan distance for this measurement. While the wires do technically cross right at the central port where they both start, this point does not count, nor does a wire count as crossing with itself.

For example, if the first wire's path is R8,U5,L5,D3, then starting from the central port (o), it goes right 8, up 5, left 5, and finally down 3:

...........
...........
...........
....+----+.
....|....|.
....|....|.
....|....|.
.........|.
.o-------+.
...........

Then, if the second wire's path is U7,R6,D4,L4, it goes up 7, right 6, down 4, and left 4:

...........
.+-----+...
.|.....|...
.|..+--X-+.
.|..|..|.|.
.|.-X--+.|.
.|..|....|.
.|.......|.
.o-------+.
...........

These wires cross at two locations (marked X), but the lower-left one is closer to the central port: its distance is 3 + 3 = 6.

Here are a few more examples:

    R75,D30,R83,U83,L12,D49,R71,U7,L72
    U62,R66,U55,R34,D71,R55,D58,R83 = distance 159
    R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
    U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = distance 135

What is the Manhattan distance from the central port to the closest intersection?

   */

  type Point = (Int, Int)

  def load(): List[String] = {
    val inputPath = "day3/input"
    import scala.io.Source
    Source
      .fromResource(inputPath)
      .getLines
      .buffered
      .take(2)
      .toList
  }

  val defaultCentral: Point = (1000, 1000)

  private def wireInstruction(central: Point, inst: String): (Point, Set[Point]) = {
    val dir = inst(0)
    val len = Integer.parseInt(inst.drop(1))
    val (x, y) = central
    dir match {
      case 'U' =>
        val newCentral = (x, y + len)
        val points = (1 to len).map(yd => (x, y + yd)).toSet
        (newCentral, points)
      case 'D' =>
        val newCentral = (x, y - len)
        val points = (1 to len).map(yd => (x, y - yd)).toSet
        (newCentral, points)
      case 'L' =>
        val newCentral = (x - len, y)
        val points = (1 to len).map(xd => (x - xd, y)).toSet
        (newCentral, points)
      case 'R' =>
        val newCentral = (x + len, y)
        val points = (1 to len).map(xd => (x + xd, y)).toSet
        (newCentral, points)
    }
  }

  def instPoints(instructions: String, central: Point = defaultCentral): Set[Point] = {
    instructions
      .split(',')
      .foldLeft((central, Set[Point]())) {
        case ((central, points), instructions) =>
          val (newCentral, newPoints) = wireInstruction(central, instructions)
          (newCentral, points ++ newPoints)
      }
      ._2
  }

  def distance(p1: Point, p2: Point): Int = {
    val (x1, y1) = p1
    val (x2, y2) = p2
    math.abs(x1 - x2) + math.abs(y1 - y2)
  }

  def smallestDistance(wire1: String, wire2: String): Int = {
    val p1 = instPoints(wire1)
    val p2 = instPoints(wire2)
    val crossing = p1.intersect(p2)
    crossing.map(distance(defaultCentral, _)).min
  }

  def part1(): Unit = {
    val List(wire1, wire2): List[String] = load()
    val result = smallestDistance(wire1, wire2)
    println(s"part 1 solution: $result")
  }

  def main(args: Array[String]): Unit = {
    part1()
  }
}

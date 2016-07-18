// Pacman A*
// https://www.hackerrank.com/challenges/pacman-astar

package hackerrank.search.packmanastar

import scala.collection._

/**
 * Plain A* search implementation
 */
object Solution {

  case class Point(row: Int, col: Int) {
    def within(size: Point) = row >= 0 && col >= 0 && row < size.row && col < size.col
    def left = Point(row, col - 1)
    def right = Point(row, col + 1)
    def up = Point(row - 1, col)
    def down = Point(row + 1, col)
  }
  case class Visited(point: Point, estimate: Int) extends Ordered[Visited] {
    override def compare(that: Visited): Int = Integer.compare(that.estimate, estimate)
  }

  val open = mutable.PriorityQueue.empty[Visited]

  val map = mutable.ArrayBuffer.empty[String]

  val distances = mutable.HashMap.empty[Point, Int]
  val cameFrom = mutable.HashMap.empty[Point, Point]

  def readInts(): Array[Int] = readLine().split("\\s+").map(_.toInt)
  def readPoint() = {
    val ints = readInts()
    assert(ints.size == 2, "Too many ints: " + ints)
    Point(ints(0), ints(1))
  }

  def main(args: Array[String]) {
    val position = readPoint()
    val food = readPoint()
    val size = readPoint()

    for (i <- 1 to size.row) {
      map += readLine()
    }

    def solve(): Seq[Point] = {
      open += Visited(position, 0)
      for (i <- 0 until size.row;
           j <- 0 until size.col) {
        distances.put(Point(i, j), Integer.MAX_VALUE)
      }
      distances.put(position, 0)

      while (open.nonEmpty) {
        val previous = open.dequeue().point
        stepFromTo(previous, previous.left)
        stepFromTo(previous, previous.right)
        stepFromTo(previous, previous.up)
        stepFromTo(previous, previous.down)
      }

      var result = List.empty[Point]
      var step = food
      result = step :: result
      while (step != position) {
        step = cameFrom(step)
        result = step :: result
      }

      result
    }

    def stepFromTo(previous: Point, point: Point) {
      if (point.within(size) && map(point.row).charAt(point.col) != '%') {
        val distance = distances(previous) + 1
        if (distance < distances(point)) {
          distances.put(point, distance)
          open += Visited(point, distance + estimateDistance(point))
          cameFrom.put(point, previous)
        }
      }
    }

    def estimateDistance(point: Point) = Math.abs(point.row - food.row) + Math.abs(point.col - food.col)

    val result = solve()
    println(result.size - 1)
    for (point <- result) {
      println(s"${point.row} ${point.col}")
    }
  }
}
package search.connectedcell

// https://www.hackerrank.com/challenges/connected-cell-in-a-grid

object Solution {

  case class Pos(row: Int, col: Int) {
    def left = Pos(row, col - 1)
    def right = Pos(row, col + 1)
    def up = Pos(row - 1, col)
    def down = Pos(row + 1, col)
  }

  def readMatrix(): Array[Array[Int]] = {
    val rows = readInt()
    val cols = readInt()
    (0 until rows).toArray.map(y => readLine().split(" ").map(_.toInt))
  }

  def solve(grid: Array[Array[Int]]): Int = {
    def cell(pos: Pos) = grid(pos.row)(pos.col)
    def isWithin(pos: Pos) = pos.row >= 0 && pos.col >= 0 && pos.row < grid.size && pos.col < grid(pos.row).size
    def nextNonFilled(pos: Pos): Option[Pos] = {
      var newPos = pos
      while (isWithin(newPos) && cell(newPos) != 1) {
        newPos = newPos.right
        if (newPos.col >= grid(newPos.row).size) {
          newPos = Pos(newPos.row + 1, 0)
        }
      }

      if (isWithin(newPos)) {
        Some(newPos)
      } else {
        None
      }
    }
    def fillRegion(pos: Pos): Int = {
      if (isWithin(pos) && cell(pos) == 1) {
        grid(pos.row)(pos.col) = 2
        1 + fillRegion(pos.left) + fillRegion(pos.right) + fillRegion(pos.up) + fillRegion(pos.down) +
          fillRegion(pos.left.up) + fillRegion(pos.right.up) + fillRegion(pos.left.down) + fillRegion(pos.right.down)
      } else {
        0
      }
    }
    def findNextRegion(nextPos: Pos): Int = {
      nextNonFilled(nextPos) match {
        case Some(startCell) => {
          val regionSize = fillRegion(startCell)
          Math.max(regionSize, findNextRegion(startCell))
        }
        case None => 0
      }
    }

    findNextRegion(Pos(0, 0))
  }

  def main(args: Array[String]) {
    val grid = readMatrix()
    println(solve(grid))
  }
}
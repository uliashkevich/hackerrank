// Synchronous Shopping
// https://www.hackerrank.com/challenges/synchronous-shopping

package search.syncshopping

import scala.collection._

/**
 *
 */
object Solution {

  case class CatPath(lastNode: Int, time: Int) {
    def movingTo(node: Int, edgeTime: Int) = CatPath(node, time + edgeTime)
  }

  case class FishSet(mask: Int, size: Int) {
    def +(fish: Int) = {
      val bit = 1 << fish
      if ((mask & bit) == 0) FishSet(mask | bit, size + 1) else this
    }

    def ++(other: FishSet) = {
      if (size == 0) {
        other
      } else if (other.size == 0) {
        this
      } else if ((mask & other.mask) == 0) {
        FishSet(mask | other.mask, size + other.size)
      } else {
        val newMask = mask | other.mask
        var bit = 1
        var newCount = 0
        for (_ <- 1 to 12) {
          if ((newMask & bit) != 0) {
            newCount += 1
          }
          bit <<= 1
        }
        FishSet(newMask, newCount)
      }
    }
  }

  case class Path(bigCat: CatPath, littleCat: CatPath, fish: FishSet) extends Ordered[Path] {
    lazy val maxTime = Math.max(bigCat.time, littleCat.time)
    lazy val currentPos = CurrentPos(bigCat.lastNode, littleCat.lastNode, fish)
    override def compare(that: Path): Int = Integer.compare(that.maxTime, this.maxTime)
  }

  case class CurrentPos(bigCat: Int, littleCat: Int, fish: FishSet)

  case class ShoppingCenter(fish: FishSet)

  type Grid = mutable.HashMap[Int, mutable.Map[Int, Int]] // Node => { Node => Time }

  case class Task(grid: Grid, shoppingCenters: Array[ShoppingCenter], K: Int, N: Int)

  def solve(task: Task): Path = {
    val queue = mutable.PriorityQueue.empty[Path]
    val visited = mutable.Set.empty[CurrentPos]

    val zeroPath = CatPath(0, 0)
    val initialPath = Path(zeroPath, zeroPath, task.shoppingCenters(0).fish)
    queue += initialPath

    while (true) {
      val path = queue.dequeue()
      if (!visited.contains(path.currentPos)) {
        visited += path.currentPos

        if (isFinalCondition(path)) {
          return path
        }

        for ((node, edgeTime) <- task.grid(path.bigCat.lastNode)) {
          val fish = path.fish ++ task.shoppingCenters(node).fish
          if (!visited.contains(CurrentPos(node, path.littleCat.lastNode, fish))) {
            val newPath = Path(path.bigCat.movingTo(node, edgeTime), path.littleCat, fish)
            queue += newPath
          }
        }

        for ((node, edgeTime) <- task.grid(path.littleCat.lastNode)) {
          val fish = path.fish ++ task.shoppingCenters(node).fish
          if (!visited.contains(CurrentPos(path.bigCat.lastNode, node, fish))) {
            val newPath = Path(path.bigCat, path.littleCat.movingTo(node, edgeTime), fish)
            queue += newPath
          }
        }
      }
    }

    def isFinalCondition(path: Path): Boolean = {
      path.fish.size == task.K && path.bigCat.lastNode == task.N && path.littleCat.lastNode == task.N
    }

    initialPath
  }

  def main(args: Array[String]): Unit = {
    val task = readTask()
    println(solve(task).maxTime)
  }

  def readInts(): Array[Int] = readLine().split("\\s+").map(_.toInt)

  def readTask() = {
    val line = readInts()
    val (n, m, k) = (line(0), line(1), line(2))
    val shoppingCenters = (1 to n).map(_ => {
      val sc = readInts()
      assert(sc(0) == sc.size - 1)
      ShoppingCenter(sc.slice(1, sc.size).foldLeft(FishSet(0, 0)) { (fishSet, fish) => fishSet + fish })
    })

    val grid = new Grid()

    for (_ <- 1 to m) {
      val edge = readInts()
      val (from, to, time) = (edge(0) - 1, edge(1) - 1, edge(2))
      val map = grid.getOrElseUpdate(from, mutable.Map.empty)
      map.put(to, time)
      val map2 = grid.getOrElseUpdate(to, mutable.Map.empty)
      map2.put(from, time)
    }

    Task(grid, shoppingCenters.toArray, k, n - 1)
  }
}

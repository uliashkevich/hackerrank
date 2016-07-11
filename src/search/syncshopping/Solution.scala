// Synchronous Shopping
// https://www.hackerrank.com/challenges/synchronous-shopping

package search.syncshopping

import scala.collection._

/**
 *
 */
object Solution {

  val Debug = false
  val PrintPath = Debug && true
  val PrintAdding = Debug && PrintPath && false
  val PrintLast = Debug && PrintPath && true
  val Pause = Debug && false
  val PrintPathFrequency = 1000000

  case class CatPath(time: Int, lastNode: Int = 0) {
    def movingTo(node: Int, edgeTime: Int) = CatPath(time + edgeTime, node)
  }

  /*
    case class CatPath(val time: Int = 0, nodes: IndexedSeq[Int] = IndexedSeq(0)) {
      def this(time: Int, node: Int) = this(time, IndexedSeq(node))
  
      lazy val lastNode = nodes(nodes.size - 1)
      def movingTo(node: Int, edgeTime: Int) = new CatPath(time + edgeTime, nodes :+ node)
    }
  */

  object FishSet {
    def apply(fish: Iterable[Int]): FishSet = {
      fish.foldLeft(FishSet(0, 0))((f, a) => f + a)
    }
  }

  case class FishSet(mask: Int, override val size: Int) extends Iterable[Int] {
    def fullyContains(other: FishSet) = mask == (mask | other.mask)

    def +(fish: Int) = {
      val bit = 1 << (fish - 1)
      if ((mask & bit) == 0) FishSet(mask | bit, size + 1) else this
    }

    def ++(other: FishSet) = {
      if (size == 0) {
        other
      } else if (other.size == 0) {
        this
      } else {
        val newMask = mask | other.mask
        var count = 1
        var x = newMask & (newMask - 1)
        while (x != 0) {
          x = x & (x - 1)
          count += 1
        }
        FishSet(newMask, count)
      }
    }

    override def iterator: Iterator[Int] = {
      new Iterator[Int] {
        var currentMask = mask
        var index = 0
        override def hasNext: Boolean = currentMask != 0
        override def next(): Int = {
          if (!hasNext) {
            throw new NoSuchElementException()
          }
          while ((currentMask & 1) == 0) {
            currentMask >>= 1
            index += 1
          }
          currentMask >>= 1
          index += 1
          index
        }
      }
    }
  }

  case class FishScore(fish: FishSet, score: Int) {
    def covers(other: FishScore) = fish.fullyContains(other.fish) && score <= other.score
  }

  case class Path(bigCat: CatPath, littleCat: CatPath, bigFish: FishSet, littleFish: FishSet, level: Int) extends Ordered[Path] {
    var bigCatEstimate = 0
    var littleCatEstimate = 0
    lazy val fishScore = FishScore(bigFish ++ littleFish, Math.max(bigCat.time + bigCatEstimate, littleCat.time + littleCatEstimate))
    def posPair = (bigCat.lastNode, littleCat.lastNode)
    def maxTime = Math.max(bigCat.time, littleCat.time)
    def fish = fishScore.fish
    def score = fishScore.score
    def hasAllFish(count: Int) = fish.size == count
    def updateEstimates(forwardDistances: Map[Int, Int], backwardDistances: Map[Int, Int]) {
      bigCatEstimate = forwardDistances(bigCat.lastNode)
      littleCatEstimate = backwardDistances(littleCat.lastNode)
    }
    override def compare(that: Path): Int = {
      val r = Integer.compare(this.fish.size, that.fish.size)
      if (r == 0) Integer.compare(that.score, this.score) else r
    }
  }

  type Grid = mutable.HashMap[Int, mutable.Map[Int, Int]] // Node => { Node => Time }

  case class Task(grid: Grid, shoppingCenters: Array[FishSet], K: Int, N: Int)

  class ScoreTable(n: Int) {
    val table: Array[Array[Option[Array[FishScore]]]] = (1 to n).toArray.map(_ => Array.fill[Option[Array[FishScore]]](n)(None))

    def update(pos: (Int, Int), arr: Array[FishScore]): Unit = {
      table(pos._1)(pos._2) = Some(arr)
    }

    def get(pos: (Int, Int)): Option[Array[FishScore]] = {
      table(pos._1)(pos._2)
    }
  }

  def printPath(path: Path, prefix: String) {
    println(s"$prefix$path; Score = ${path.score}; Distance = ${path.maxTime}")
  }

  def buildDistanceMatrix(n: Int, grid: Grid, startNode: Int): Array[Int] = {
    val matrix = Array.fill(n)(Integer.MAX_VALUE)
    matrix(startNode) = 0
    var frontier = Set(startNode)
    while (frontier.nonEmpty) {
      val newFrontier = for {
        node <- frontier
        distance = matrix(node)
        (neighbour, edge) <- grid(node)
        newDistance = distance + edge
        if newDistance < matrix(neighbour)
      } yield {
        matrix(neighbour) = newDistance
        neighbour
      }
      frontier = newFrontier.toSet
    }
    matrix
  }

  def buildFishMap(task: Task): Map[Int, IndexedSeq[Int]] = {
    var result = Map.empty[Int, IndexedSeq[Int]]
    for {
      i <- 0 until task.N
      fish = task.shoppingCenters(i)
      fishNumber <- fish
    } {
      result.get(fishNumber) match {
        case Some(nodes) =>
          result += ((fishNumber, nodes :+ i))
        case None =>
          result += ((fishNumber, IndexedSeq(i)))
      }
    }
    result
  }

  def solve(task: Task, distanceMatrix: Array[Array[Int]], fishMap: Map[Int, IndexedSeq[Int]]): Path = {
    def isFinalCondition(path: Path): Boolean = {
      path.hasAllFish(task.K) && path.bigCat.lastNode == task.N - 1 && path.littleCat.lastNode == task.N - 1
    }

    val queue = mutable.PriorityQueue.empty[Path]

    var finalResult = Path(CatPath(Integer.MAX_VALUE), CatPath(0), FishSet(0, 0), FishSet(0, 0), 0)

    def queuePath(path: Path): Unit = {
      path.bigCatEstimate = distanceMatrix(path.bigCat.lastNode)(task.N - 1)
      path.littleCatEstimate = distanceMatrix(path.littleCat.lastNode)(task.N - 1)
      if (path.score < finalResult.maxTime) {
        if (isFinalCondition(path)) {
          if (PrintLast) {
            println(s"Finish: $path; Distance = ${path.maxTime}")
          }
          finalResult = path
        } else {
          queue += path
        }
      }
    }

    queuePath(Path(CatPath(0, 0), CatPath(0, 0), task.shoppingCenters(0), task.shoppingCenters(0), 0))

    var count = 0
    while (queue.nonEmpty) {
      val path = queue.dequeue()
      if (path.score < finalResult.maxTime) {
        if (isFinalCondition(path)) {
          if (PrintPath) {
            println(s"Finish: $path; Distance = ${path.maxTime}")
          }
          finalResult = path
        } else {
          if (PrintPath && (count % PrintPathFrequency) == 0) {
            printPath(path, "")
          }
          count += 1

          val missingFish = (1 to task.K).toSet -- path.fish

          if (missingFish.isEmpty) {
            queuePath(Path(
              path.bigCat.movingTo(task.N - 1, distanceMatrix(path.bigCat.lastNode)(task.N - 1)),
              path.littleCat.movingTo(task.N - 1, distanceMatrix(path.littleCat.lastNode)(task.N - 1)),
              path.bigFish,
              path.littleFish,
              1
            ))
          } else {
            val targetNodes = (for {
              fishNumber <- missingFish
              fishNode <- fishMap(fishNumber)
            } yield fishNode).toSet

            for (node <- targetNodes) {
              if (path.level == 0) {
                queuePath(Path(
                  path.bigCat.movingTo(node, distanceMatrix(path.bigCat.lastNode)(node)),
                  path.littleCat,
                  path.bigFish ++ task.shoppingCenters(node),
                  path.littleFish,
                  1))
              } else {
                queuePath(Path(
                  path.bigCat,
                  path.littleCat.movingTo(node, distanceMatrix(path.littleCat.lastNode)(node)),
                  path.bigFish,
                  path.littleFish ++ task.shoppingCenters(node),
                  0))
              }
            }
          }
        }
      }
    }

    finalResult
  }

  def main(args: Array[String]): Unit = {
    val task = readTask()

    val startTime = System.currentTimeMillis()

    val forwardDistances = buildDistanceMatrix(task.N, task.grid, task.N - 1)
    val backwardDistances = buildDistanceMatrix(task.N, task.grid, 0)

    val distanceMatrix = (0 until task.N).toArray.map(i => buildDistanceMatrix(task.N, task.grid, i))
    val fishMap = buildFishMap(task)

    if (Debug) {
      println(s"Fish map: $fishMap")
    }

    val result = solve(task, distanceMatrix, fishMap)

    if (PrintLast) {
      println("Solution: " + result)
      val time = System.currentTimeMillis() - startTime
      println(s"Time: ${time / 1000.0} seconds")
    }

    println(result.maxTime)
  }

  def readTask() = {
    def readInts(): Array[Int] = readLine().split("\\s+").map(_.toInt)

    val line = readInts()
    val (n, m, k) = (line(0), line(1), line(2))
    val shoppingCenters = (1 to n).map(_ => {
      val sc = readInts()
      assert(sc(0) == sc.size - 1)
      sc.slice(1, sc.size).foldLeft(FishSet(0, 0)) { (fishSet, fish) => fishSet + fish }
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

    Task(grid, shoppingCenters.toArray, k, n)
  }
}


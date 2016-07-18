package interview.domdistance

import scala.collection._

// Object Model

case class Tag(name: String, id: Option[String] = None, classes: Set[String] = Set.empty) {
  def diff(that: Tag) = {
    var count = 0
    if (name != that.name) {
      count += 1
    }
    if (id != that.id) {
      count += (if (id.isEmpty || that.id.isEmpty) 1 else 2)
    }
    if (classes != that.classes) {
      count += (classes -- that.classes).size + (that.classes -- classes).size
    }
    count
  }
}

object Tag {
  val Empty = Tag("")

  def parsePath(input: String): IndexedSeq[Tag] = {
    def parseClasses(classes: String) = classes.split("\\.").toSet
    for {
      element <- input.split("\\s+")
    } yield {
      (element.indexOf('#'), element.indexOf('.')) match {
        case (-1, -1) => Tag(element)
        case (-1, classPos) =>
          Tag(element.substring(0, classPos),
            None,
            parseClasses(element.substring(classPos + 1, element.size)))
        case (idPos, -1) =>
          Tag(element.substring(0, idPos),
            Some(element.substring(idPos + 1, element.size)))
        case (idPos, classPos) =>
          Tag(element.substring(0, idPos),
            Some(element.substring(idPos + 1, classPos)),
            parseClasses(element.substring(classPos + 1, element.size)))
      }
    }
  }
}

// Solution
// Levenshtein distance between DOM element paths
object Solution {
  def distance(s: IndexedSeq[Tag], t: IndexedSeq[Tag]): Int = {
    val insertionCost = new Array[Int](t.size + 1)
    for (j <- 0 until t.size) {
      insertionCost(j) = t(j).diff(Tag.Empty)
    }

    val prevRow = new Array[Int](t.size + 1)
    val nextRow = new Array[Int](t.size + 1)

    for (j <- 0 until t.size) {
      prevRow(j + 1) = prevRow(j) + insertionCost(j)
    }

    for (i <- 0 until s.size) {
      nextRow(0) = prevRow(0) + 1
      for (j <- 0 until t.size) {
        val insertion = nextRow(j) + insertionCost(j)
        val deletion = prevRow(j + 1) + 1
        val change = prevRow(j) + s(i).diff(t(j))
        nextRow(j + 1) = Math.min(insertion, Math.min(deletion, change))
      }

      for (j <- 0 to t.size) {
        prevRow(j) = nextRow(j)
      }
    }

    nextRow(t.size)
  }
}

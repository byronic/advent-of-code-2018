import scala.io.Source

object Coordinatron {
  val bufferedSource = Source.fromFile("dataset")
  val lines = bufferedSource.getLines.toList
  bufferedSource.close

  def absoluteValue(n: Int): Int = {
    if (n < 0) n * -1 else n
  }

  def distance(p1: (Int, Int), p2: (Int, Int)): Int = {
    absoluteValue(p1._1 - p2._1) + absoluteValue(p1._2 - p2._2)
  }

  def max(a: Int, b: Int): Int = {
    if (a > b) a else b
  }

  def min(a: Int, b: Int): Int = {
    if (a < b) a else b
  }

  // finds the bounds as follows: x min, x max, y min, y max
  def bounds(l: List[(Int, Int)]): (Int, Int, Int, Int) = {
    @annotation.tailrec
    def go(l: List[(Int, Int)], xMin: Int, xMax: Int, yMin: Int, yMax: Int): (Int, Int, Int, Int) = {
      if (l.isEmpty) (xMin, xMax, yMin, yMax)
      else go(l.tail, min(l.head._1, xMin), max(l.head._1, xMax), min(l.head._2, yMin), max(l.head._2, yMax))
    }
    go(l.tail, l.head._1, l.head._1, l.head._2, l.head._2)
  }

  // returns the list's closest point (or one of them), and true if equidistant to multiple points or false otherwise
  def findClosestPoint(p: (Int, Int), l: List[(Int, Int)]): ((Int, Int), Boolean) = {
    def go(p: (Int, Int), l: List[(Int, Int)], s: Int, c: (Int, Int), e: Boolean): ((Int, Int), Boolean) = {
      if (l.isEmpty) (c, e)
      else {
        val currentDistance: Int = distance(p, l.head)
        val shortest: Int = min(currentDistance, s)
        val closer: (Int, Int) = if (shortest == s) c else l.head
        /* if shortest doesn't equal s, it changed; equi must become false
         if currentDistance equals s, we have seen this distance before; equi must become true
         in all other cases equi must remain at the value of e */
        val equidistant: Boolean = if (shortest != s) false else if (currentDistance == s) true else e
        go(p, l.tail, shortest, closer, equidistant)
      }
    }

    go(p, l.tail, distance(p, l.head), l.head, false)
  }



  // do a loop from all bounds minus one, calculate each point distance, then be done
  // also, save a list of infinites (closest points to outside border are hitting infinite space)
  def makeMap(l: List[(Int, Int)], bounds: (Int, Int, Int, Int)): (Map[(Int, Int), Int], List[(Int, Int)]) = {
    def updateMap(m: Map[(Int, Int), Int], p: ((Int, Int), Boolean)): Map[(Int, Int), Int] = {
      if (p._2) m
      else m.updated(p._1, m.get(p._1) match {
          case Some(i) => i + 1
          case None => 1
        })
    }

    def updateList(l: List[(Int, Int)], p: ((Int, Int), Boolean)): List[(Int, Int)] = {
      if (p._2 || l.contains(p)) l else p._1 :: l
    }

    def go(l: List[(Int, Int)], bounds: (Int, Int, Int, Int), x: Int, y: Int, m: Map[(Int, Int), Int], infinites: List[(Int, Int)]): (Map[(Int, Int), Int], List[(Int, Int)]) = {
      if (y > bounds._4 + 2) (m, infinites)
      else {
        val currentX: Int = if (x > bounds._2 + 1) bounds._1 - 1 else x
        val currentY: Int = if (currentX < x) y + 1 else y
        val closestPoint: ((Int, Int), Boolean) = findClosestPoint((currentX, currentY), l)
        val is: List[(Int, Int)] = if (x < bounds._1 || x > bounds._2 || y < bounds._3 || y > bounds._4) updateList(infinites, closestPoint) else infinites
        go(l, bounds, currentX + 1, currentY, updateMap(m, closestPoint), is)
      }
    }

    go(l, bounds, bounds._1 - 1, bounds._3 - 1, Map(), List())
  }

  def findGreatestArea(m: Map[(Int, Int), Int], is: List[(Int, Int)]): Int = {
    def go(m: Map[(Int, Int), Int], is: List[(Int, Int)], acc: Int): Int = {
      if (m.isEmpty) acc
      else go(m - m.head._1, is, if (is.contains(m.head._1) || max(m.head._2, acc) == acc) acc else m.head._2)
    }
    go(m, is, 0)
  }

  def findSafestCoordinates(ps: List[(Int, Int)], bounds: (Int, Int, Int, Int)): Int = {
    def sumDistances(p: (Int, Int), ps: List[(Int, Int)]): Int = {
      def go(p: (Int, Int), ps: List[(Int, Int)], acc: Int): Int = {
        if (ps.isEmpty || acc > 10000) acc
        else go(p, ps.tail, acc + distance(p, ps.head))
      }
      println(s"Summing distances for ${p}")
      go(p, ps, 0)
    }

    def go(ps: List[(Int, Int)], bounds: (Int, Int, Int, Int), x: Int, y: Int, acc: Int): Int = {
      val currentX = if (x > bounds._2) 0 else x
      val currentY = if (x > currentX) y + 1 else y
      if (y > bounds._4) acc
      else go(ps, bounds, currentX + 1, currentY, if (sumDistances((currentX, currentY), ps) < 10000) 1 + acc else acc)
    }

    go(ps, bounds, bounds._1, bounds._3, 0)
  }

  def makePointList(l: List[String]): List[(Int, Int)] = {
    def go(l: List[String], ps: List[(Int, Int)]): List[(Int, Int)] = {
      if (l.isEmpty) ps
      else {
        val split: Array[String] = l.head.split(",")
        go(l.tail, (split(0).toInt, split(1).trim.toInt) :: ps)
      }
    }
    go(l, List())
  }


  def main(args: Array[String]): Unit = {
    val points: List[(Int, Int)] = makePointList(lines)
    val res: (Map[(Int, Int), Int], List[(Int, Int)]) = makeMap(points, bounds(points))
    println(findGreatestArea(res._1, res._2))
    println(findSafestCoordinates(points, bounds(points)))
  }
}

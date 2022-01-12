import scala.io.Source

object Coordinatron {
  case class Point(x: Int, y: Int)
  case class MapPoint(point: Point, isInfinite: Boolean)
  case class Bounds(minX: Int, maxX: Int, minY: Int, maxY: Int)

  def distance(p1: Point, p2: Point): Int = {
    (p1.x - p2.x).abs + (p1.y - p2.y).abs
  }

  // finds the bounds as follows: x min, x max, y min, y max
  def bounds(l: List[Point]): Bounds = {
    @annotation.tailrec
    def go(l: List[Point], xMin: Int, xMax: Int, yMin: Int, yMax: Int): Bounds = {
      if (l.isEmpty) Bounds(xMin, xMax, yMin, yMax)
      else go(l.tail, l.head.x.min(xMin), l.head.x.max(xMax), l.head.y.min(yMin), l.head.y.max(yMax))
    }
    go(l.tail, l.head.x, l.head.x, l.head.y, l.head.y)
  }

  // returns the list's closest point (or one of them), and true if equidistant to multiple points or false otherwise
  def findClosestPoint(point: Point, l: List[Point]): MapPoint = {
    def go(l: List[Point], shortestDistance: Int, closestPoint: Point, equidistant: Boolean): MapPoint = {
      if (l.isEmpty) MapPoint(closestPoint, equidistant)
      else {
        val currentDistance: Int = distance(point, l.head)
        val shorter: Int = currentDistance.min(shortestDistance)
        val closer: Point = if (shorter == shortestDistance) closestPoint else l.head
        /* if shorter doesn't equal shortestDistance passed to this iteration of go, it changed;
         equidistant must become false
         if currentDistance equals shortestDistance, we have seen this distance before; equidistant must become true
         in all other cases equidistant must remain at the value passed to this iteration of go */
        val newEquidistant: Boolean = if (shorter != shortestDistance) false else if (currentDistance == shortestDistance) true else equidistant
        go(l.tail, shorter, closer, newEquidistant)
      }
    }

    go(l.tail, distance(point, l.head), l.head, false)
  }

  // do a loop from all bounds minus one, calculate each point distance, then be done
  // also, save a list of infinites (closest points to outside border are hitting infinite space)
  def makeMap(l: List[Point], bounds: Bounds): (Map[Point, Int], List[Point]) = {
    def updateMap(m: Map[Point, Int], p: MapPoint): Map[Point, Int] = {
      if (p.isInfinite) m
      else m.updated(p.point, m.get(p.point) match {
          case Some(i) => i + 1
          case None => 1
        })
    }

    // adds a point to the list if a) it's not infinite and b) it's not already in the list (no duplicates!)
    def updateList(l: List[Point], p: MapPoint): List[Point] = {
      if (p.isInfinite || l.contains(p)) l else p.point :: l
    }

    def go(x: Int, y: Int, m: Map[Point, Int], infinites: List[Point]): (Map[Point, Int], List[Point]) = {
      if (y > bounds.maxY + 2) (m, infinites)
      else {
        val currentX: Int = if (x > bounds.maxX + 1) bounds.minX - 1 else x
        val currentY: Int = if (currentX < x) y + 1 else y
        val closestPoint: MapPoint = findClosestPoint(Point(currentX, currentY), l)
        val infinite: List[Point] = if (x < bounds.minX || x > bounds.maxX || y < bounds.minY || y > bounds.maxY) updateList(infinites, closestPoint) else infinites
        go(currentX + 1, currentY, updateMap(m, closestPoint), infinite)
      }
    }

    go(bounds.minX - 1, bounds.minY - 1, Map(), List())
  }

  def findGreatestArea(m: Map[Point, Int], infinites: List[Point]): Int = {
    def go(m: Map[Point, Int], acc: Int): Int = {
      if (m.isEmpty) acc
      else go(m - m.head._1, if (infinites.contains(m.head._1) || m.head._2.max(acc) == acc) acc else m.head._2)
    }
    go(m, 0)
  }

  def findSafestCoordinates(points: List[Point], bounds: Bounds): Int = {
    def sumDistances(p: Point, ps: List[Point]): Int = {
      def go(ps: List[Point], acc: Int): Int = {
        if (ps.isEmpty || acc > 10000) acc
        else go(ps.tail, acc + distance(p, ps.head))
      }
      go(points, 0)
    }

    def go(x: Int, y: Int, acc: Int): Int = {
      val currentX = if (x > bounds.maxX) 0 else x
      val currentY = if (x > currentX) y + 1 else y
      if (y > bounds.maxY) acc
      else go(currentX + 1, currentY, if (sumDistances(Point(currentX, currentY), points) < 10000) 1 + acc else acc)
    }

    go(bounds.minX, bounds.minY, 0)
  }

  def makePointList(l: List[String]): List[Point] = {
    def go(l: List[String], ps: List[Point]): List[Point] = {
      if (l.isEmpty) ps
      else {
        val split: Array[String] = l.head.split(",")
        go(l.tail, Point(split(0).toInt, split(1).trim.toInt) :: ps)
      }
    }
    go(l, List())
  }


  def main(args: Array[String]): Unit = {
    val bufferedSource = Source.fromFile("dataset")
    val lines = bufferedSource.getLines.toList
    bufferedSource.close
    val points: List[Point] = makePointList(lines)
    val res: (Map[Point, Int], List[Point]) = makeMap(points, bounds(points))
    println(findGreatestArea(res._1, res._2))
    println(findSafestCoordinates(points, bounds(points)))
  }
}

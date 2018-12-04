import scala.io.Source

object Frequency {
val bufferedSource = Source.fromFile("dataset")
val lines = bufferedSource.getLines.toList
bufferedSource.close

def frequency(l: List[String]): Int = {
  @annotation.tailrec
  def go(l: List[String], curr: Int): Int = {
    if (l.isEmpty) curr
    else go(l.tail, curr + l.head.toInt)
  }
  go(l, 0)
}

def firstTwice(l: List[String]): Int = {
  def go(l: List[String], i: Int, fs: List[Int], curr: Int): Int = {
    val index: Int = if (i == l.length) 0 else i
    if (fs.contains(curr)) curr
    else go(l, index + 1, (curr) :: fs, curr + l(index).toInt)
  }
  go(l, 0, List(), 0)
}

frequency(lines)
firstTwice(lines)
}

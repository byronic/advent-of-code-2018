import scala.io.Source

object Reactor {
val bufferedSource = Source.fromFile("dataset")
val lines = bufferedSource.getLines.toList
bufferedSource.close
 
  def equalsAndDifferentCase(a: Char, b: Char): Boolean = {
    a.toUpper == b.toUpper && a.isLower != b.isLower
  }

  def reactHelper(v: String): String = {
    if (v.length <= 1) return v
    else if (equalsAndDifferentCase(v.head, v.tail.head)) {
        reactHelper(v.tail.tail)
    } else {
      v.head + reactHelper(v.tail)
    }
  }

  def react(v: String, n: Int): String = {
    if (v.length <= n) reactHelper(v)
    else reactHelper(v.substring(0, n)) + react(v.substring(n), n)
  }

  def reactor(v: String, chunk: Int, length: Int): String = {
    val sub = react(v, chunk)
    if (sub.length == length) sub
    else reactor(sub, chunk, sub.length)
  }
  
def main(args: Array[String]): Unit = {
val result = reactor(lines.mkString(""), 25, 0)
println(result)
println(result.length )

}
}

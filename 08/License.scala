import scala.io.Source

object License {
  case class Node(children: List[Node], values: List[Int])

  def getArrayOfInts(s: String): Array[Int] = {
    s.split(" ").map( (x: String) => {
      x.toInt
    })
  }

  def makeNodesFromArray(a: Array[Int]): List[Node] = {
    def makeNode(a: Array[Int], childrenSoFar: List[Node]): (List[Node], Array[Int]) = {
      val children: Int = a(0)
      val metaEntries: Int = a(1)
      if (children == 0) {
        val resultNode: Node = Node(List(), a.slice(2, metaEntries + 2).toList)
        val resultArray: Array[Int] = a.slice(metaEntries + 2, a.length)
        (List(resultNode), resultArray)
      }
      else if(childrenSoFar.size == children) (List(Node(childrenSoFar, a.slice(2, metaEntries + 2).toList)), a.slice(metaEntries + 2, a.length))
      else {
        val result: (List[Node], Array[Int]) = makeNode(a.tail.tail, List())
        val newChildren: List[Node] = childrenSoFar ::: result._1
        val resultArray: Array[Int] = a(0) +: a(1) +: result._2
        makeNode(resultArray, newChildren)
      }
    }
    makeNode(a, List())._1
  }

  // the part one answer
  def sumMetaEntries(l: List[Node]): Int = {
    def sumList(ints: List[Int], acc: Int): Int = {
      if (ints.isEmpty) acc
      else sumList(ints.tail, acc + ints.head)
    }
    def go(l: List[Node], acc: Int): Int = {
      if (l.isEmpty) acc
      else go(l.tail, acc + sumList(l.head.values, 0) + go(l.head.children, 0))
    }
    go(l, 0)
  }

  def valueOfRootNode(l: List[Node]): Int = {
    def sumList(ints: List[Int], acc: Int): Int = {
      if (ints.isEmpty) acc
      else sumList(ints.tail, acc + ints.head)
    }
    def sumChildrenMeta(children: List[Node], values: List[Int], acc: Int): Int = {
      if (values.isEmpty) acc
      else {
        val toAdd: Int = children.lift(values.head - 1) match {
          case Some(x: Node) => if (x.children.isEmpty) sumList(x.values, 0) else sumChildrenMeta(x.children, x.values, 0)
          case None => 0
        }
        sumChildrenMeta(children, values.tail, acc + toAdd)
      }
    }
    sumChildrenMeta(l.head.children, l.head.values, 0)
  }

  def main(args: Array[String]): Unit = {
    val bufferedSource = Source.fromFile("dataset")
    val lines = bufferedSource.getLines.toList
    bufferedSource.close
    val tree = makeNodesFromArray(getArrayOfInts(lines.head))
    println(sumMetaEntries(tree))
    println(valueOfRootNode(tree))
  }
}

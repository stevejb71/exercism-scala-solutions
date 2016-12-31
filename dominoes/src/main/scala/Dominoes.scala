import scala.annotation.tailrec

case class Dominoe(val d: (Int, Int)) extends AnyVal {
  def isSymmetric: Boolean = d._1 == d._2
}

case class Graph(private val matrix: Long) extends AnyVal {
  def set(x1: Int, x2: Int): Graph = set1(x1, x2).set1(x2, x1)
  def isEmpty: Boolean = matrix == 0
  def unset(x1: Int, x2: Int): Graph = unset1(x1, x2).unset1(x2, x1)
  def pick: Option[(Graph, Dominoe)] = {
    for {
      pickedRowIndex <- (0 until 8).find(r => row(r) != 0)
      pickedRow = row(pickedRowIndex)
      pickedColIndex = Graph.hibit(pickedRow)
      graph = unset(pickedRowIndex, pickedColIndex)
    } yield (graph, Dominoe((pickedRowIndex, pickedColIndex)))
  }

  private def set1(x1: Int, x2: Int): Graph = Graph(matrix | 1L << (x1 * 8 + x2))
  private def unset1(x1: Int, x2: Int): Graph = Graph(matrix & ~(1L << (x1 * 8 + x2)))
  def row(x1: Int): Byte = ((matrix & (0xFF << 8 * x1)) >> 8 * x1).toByte
}

object Graph {
  def ofList(xs: List[(Int, Int)]): Graph = xs.foldLeft(Graph(0)){case (g, (x, y)) => g.set(x,y)}
  def hibit(value: Byte): Byte = {
    @tailrec def go(result: Int)(mask: Int): Byte = {
      if((value & mask) != 0) {
        result.toByte
      } else {
        go(result - 1)(mask >> 1)
      }
    }
    go(6)(64)
  }
}

object Dominoes {
  def chain(dominoes: List[(Int, Int)]): Option[List[(Int, Int)]] = {
    val graph = Graph.ofList(dominoes)
    val picked = graph.pick
    if(picked.isEmpty) {
      Some(Nil)
    } else {
      val (_, d) = picked.get
      if(d.isSymmetric) {
        Some(List(d.d))
      } else {
        None
      }
    }
  }
}

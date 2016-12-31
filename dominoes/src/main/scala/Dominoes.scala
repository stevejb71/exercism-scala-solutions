import scala.annotation.tailrec

case class Dominoe(d: (Int, Int)) extends AnyVal {
  def isSymmetric: Boolean = d._1 == d._2
  def _1 = d._1
  def _2 = d._2
  def swap = Dominoe(d.swap)
}

case class Graph(private val matrix: Long) extends AnyVal {
  def set(x1: Int, x2: Int): Graph = set1(x1, x2).set1(x2, x1)
  def isEmpty: Boolean = matrix == 0
  def unset(x1: Int, x2: Int): Graph = unset1(x1, x2).unset1(x2, x1)
  def pickAny: Option[(Graph, Dominoe)] = {
    for {
      pickedRowIndex <- (0 until 8).find(r => row(r) != 0)
      pickedRow = row(pickedRowIndex)
      pickedColIndex = Graph.hibit(pickedRow)
      graph = unset(pickedRowIndex, pickedColIndex)
    } yield (graph, Dominoe((pickedRowIndex, pickedColIndex)))
  }
  def pickMatching(n: Int): Option[(Graph, Dominoe)] = {
    val nthRow = row(n)
    if(nthRow != 0) {
      val colIndex = Graph.hibit(nthRow)
      val g = unset(n, colIndex)
      Some((g, Dominoe(n, colIndex)))
    } else {
      None
    }
  }
  private def set1(x1: Int, x2: Int): Graph = Graph(matrix | 1L << (x1 * 8 + x2))
  private def unset1(x1: Int, x2: Int): Graph = Graph(matrix & ~(1L << (x1 * 8 + x2)))
  private def row(x1: Int): Byte = ((matrix & (0xFF << 8 * x1)) >> 8 * x1).toByte
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
  // Use Vector for snocs
  def chain(dominoes: List[(Int, Int)]): Option[List[(Int, Int)]] = {
    def findChain(g: Graph, chain: List[(Int, Int)]): List[(Int, Int)] = {
      val first = chain.head._1
      val firstMatch = g.pickMatching(first)
      if(firstMatch.isEmpty) {
        val last = chain.last._2
        val lastMatch = g.pickMatching(last)
        if(lastMatch.isEmpty) {
          chain
        } else {
          val (g1, d1) = lastMatch.get
          findChain(g1, chain :+ d1.d)
        }
      } else {
        val (g1, d1) = firstMatch.get
        val d = d1.swap
        findChain(g1, d.d :: chain)
      }
    }
    if(dominoes.isEmpty) {
      Some(Nil)
    } else {
      val chain = findChain(Graph.ofList(dominoes.tail), List(dominoes.head))
      if(chain.length == 1 && !Dominoe(chain.head).isSymmetric) {
        None
      } else {
        Some(chain)
      }
    }
  }
}

import scala.annotation.tailrec

case class Dominoe(d: (Int, Int)) extends AnyVal {
  def isSymmetric: Boolean = d._1 == d._2
  def _1 = d._1
  def _2 = d._2
  def swap = Dominoe(d.swap)
}

case class Graph() {
  private val matrix: Array[Array[Int]] = Array.fill(6, 6)(0)
  private var size = 0

  def set(x1: Int, x2: Int): Unit = {
    set1(x1, x2)
    set1(x2, x1)
    size += 1
  }
  def isEmpty: Boolean = size == 0
  def unset(x1: Int, x2: Int): Unit = {
    unset1(x1, x2)
    unset1(x2, x1)
  }
  def pickMatching(n: Int): Option[Dominoe] = {
    val nthRow = matrix(n)
    val colIndex = nthRow.indexWhere(_ != 0)
    if(colIndex != -1) {
      unset(n, colIndex)
      Some(Dominoe(n, colIndex))
    } else {
      None
    }
  }
  private def set1(x1: Int, x2: Int): Unit = matrix(x1)(x2) += 1
  private def unset1(x1: Int, x2: Int): Unit = matrix(x1)(x2) -= 1
}

object Graph {
  def ofList(xs: List[(Int, Int)]): Graph = {
    val g = Graph()
    xs.foreach {case (x1, x2) => g.set(x1, x2)}
    g
  }
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
    @tailrec def findChain(g: Graph, chain: List[(Int, Int)]): List[(Int, Int)] = {
      val first = chain.head._1
      val firstMatch = g.pickMatching(first)
      if(firstMatch.isEmpty) {
        val last = chain.last._2
        val lastMatch = g.pickMatching(last)
        if(lastMatch.isEmpty) {
          chain
        } else {
          val d1 = lastMatch.get
          findChain(g, chain :+ d1.d)
        }
      } else {
        val d1 = firstMatch.get
        val d = d1.swap
        findChain(g, d.d :: chain)
      }
    }
    if(dominoes.isEmpty) {
      Some(Nil)
    } else {
      val chain = findChain(Graph.ofList(dominoes.tail), List(dominoes.head))
      if(chain.length == 1 && !Dominoe(chain.head).isSymmetric) {
        None
      } else {
        if (isValidChain(chain))
          if (chain.length == dominoes.length) {
            Some(chain)
          } else {
            None
          }
        else {
          if (chain.length == dominoes.length) {
            None
          } else {
            None
          }
        }
      }
    }
  }

  private def isValidChain(ds: List[(Int, Int)]): Boolean = {
    val d1 = ds.head._1
    val d2 = ds.last._2
    d1 == d2
  }
}

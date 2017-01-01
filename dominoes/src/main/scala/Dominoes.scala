import scala.annotation.tailrec

case class Dominoe(d: (Int, Int)) extends AnyVal {
  def isSymmetric: Boolean = d._1 == d._2
  def swap = Dominoe(d.swap)
}

case class Graph(private val matrix: Array[Array[Int]] = Array.fill(7, 7)(0)) {
  private var size = 0
  def copy: Graph = {
    val c = Graph(matrix.clone().map(_.clone()))
    c.size = this.size
    c
  }
  def set(x1: Int, x2: Int): Unit = {
    def set1(x1: Int, x2: Int): Unit = matrix(x1)(x2) += 1
    set1(x1, x2)
    set1(x2, x1)
    size += 1
  }
  def isEmpty: Boolean = size == 0
  def unset(x1: Int, x2: Int): Unit = {
    def unset1(x1: Int, x2: Int): Unit = matrix(x1)(x2) -= 1
    unset1(x1, x2)
    unset1(x2, x1)
    size -= 1
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
}

object Graph {
  def ofList(xs: List[(Int, Int)]): Graph = {
    val g = Graph()
    xs.foreach {case (x1, x2) => g.set(x1, x2)}
    g
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
      val g = Graph.ofList(dominoes.tail)
      val chain = findChain(g, List(dominoes.head))
      if (chain.length == dominoes.length) {
        if (isValidChain(chain)) {
          Some(chain)
        } else {
          None
        }
      } else {
        var i = -1
        var chainFound: Seq[(Int, Int)] = Seq()
        while(i < chain.length - 1 && chainFound.size <= 1) {
          i += 1
          val insertAt = chain(i)
          val g1 = g.copy
          chainFound = findChain(g1, List(insertAt))
        }
        if(chainFound.size > 1) {
          val (before, after) = chain.splitAt(i + 1)
          val extended = before ++ chainFound.tail ++ after
          Some(extended)
        } else {
          None
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

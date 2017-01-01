import scala.annotation.tailrec

case class AdjacencyMatrix(private val matrix: Array[Array[Int]] = Array.fill(7, 7)(0)) {
  private var size = 0
  def copy: AdjacencyMatrix = {
    val c = AdjacencyMatrix(matrix.clone().map(_.clone()))
    c.size = this.size
    c
  }
  def set(x1: Int, x2: Int): Unit = {
    def set1(x1: Int, x2: Int): Unit = matrix(x1)(x2) += 1
    set1(x1, x2)
    set1(x2, x1)
    size += 1
  }
  def unset(x1: Int, x2: Int): Unit = {
    def unset1(x1: Int, x2: Int): Unit = matrix(x1)(x2) -= 1
    unset1(x1, x2)
    unset1(x2, x1)
    size -= 1
  }
  def pickMatching(n: Int): Option[(Int, Int)] = {
    val nthRow = matrix(n)
    val colIndex = nthRow.indexWhere(_ != 0)
    if(colIndex != -1) {
      unset(n, colIndex)
      Some((n, colIndex))
    } else {
      None
    }
  }
}

object AdjacencyMatrix {
  def ofList(xs: List[(Int, Int)]): AdjacencyMatrix = {
    val g = AdjacencyMatrix()
    xs.foreach {case (x1, x2) => g.set(x1, x2)}
    g
  }
}

object Dominoes {
  // Use Vector for snocs
  def chain(dominoes: List[(Int, Int)]): Option[List[(Int, Int)]] = {
    @tailrec def findChain(g: AdjacencyMatrix, chain: List[(Int, Int)]): List[(Int, Int)] = {
      val firstMatch = g.pickMatching(chain.head._1)
      if(firstMatch.isEmpty) {
        val lastMatch = g.pickMatching(chain.last._2)
        if(lastMatch.isEmpty) {
          chain
        } else {
          findChain(g, chain :+ lastMatch.get)
        }
      } else {
        findChain(g, firstMatch.get.swap :: chain)
      }
    }
    if(dominoes.isEmpty) {
      Some(Nil)
    } else {
      val g = AdjacencyMatrix.ofList(dominoes.tail)
      val chain = findChain(g, List(dominoes.head))
      var keepGoing = true
      var extended = chain
      var currGraph = g
      while(keepGoing) {
        var i = -1
        var chainFound: Seq[(Int, Int)] = Seq()
        while (i < extended.length - 1 && chainFound.size <= 1) {
          i += 1
          val insertAt = extended(i)
          val g1 = currGraph.copy
          chainFound = findChain(g1, List(insertAt))
          if (chainFound.size > 1) {
            currGraph = g1
          }
        }
        if (chainFound.size > 1) {
          val (before, after) = extended.splitAt(i + 1)
          extended = before ++ chainFound.tail ++ after
          keepGoing = extended.length < dominoes.length
        } else {
          keepGoing = false
        }
      }
      if(extended.length == dominoes.length && isValidChain(extended)) {
        Some(extended)
      } else {
        None
      }
    }
  }

  private def isValidChain(ds: List[(Int, Int)]): Boolean = {
    val d1 = ds.head._1
    val d2 = ds.last._2
    d1 == d2
  }
}

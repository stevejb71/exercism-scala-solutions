import scala.annotation.tailrec

case class AdjacencyMatrix(private val matrix: Vector[Int] = Vector.fill(7 * 7)(0)) extends AnyVal {
  def set(x1: Int, x2: Int): AdjacencyMatrix = modify(x1, x2, 1).modify(x2, x1, 1)
  def unset(x1: Int, x2: Int): AdjacencyMatrix = modify(x1, x2, -1).modify(x2, x1, -1)
  def pickMatching(n: Int): Option[(AdjacencyMatrix, (Int, Int))] = {
    val colIndex = matrix.slice(n * 7, n * 7 + 7).indexWhere(_ != 0)
    if(colIndex != -1) {
      Some((unset(n, colIndex), (n, colIndex)))
    } else {
      None
    }
  }
  private def modify(x1: Int, x2: Int, inc: Int): AdjacencyMatrix = {
    val i = x1 * 7 + x2
    AdjacencyMatrix(matrix.updated(i, matrix(i) + inc))
  }
}

object AdjacencyMatrix {
  def ofList(xs: List[(Int, Int)]): AdjacencyMatrix = xs.foldLeft(AdjacencyMatrix()){case (am, (x1, x2)) => am.set(x1, x2)}
}

object Dominoes {
  def chain(dominoes: List[(Int, Int)]): Option[List[(Int, Int)]] = {
    @tailrec def findChain(g: AdjacencyMatrix, chain: List[(Int, Int)]): (AdjacencyMatrix, List[(Int, Int)]) = {
      (g.pickMatching(chain.head._1), g.pickMatching(chain.last._2)) match {
        case (Some((g1, first)), _) => findChain(g1, first.swap :: chain)
        case (_, Some((g1, last))) => findChain(g1, chain :+ last)
        case _ => (g, chain)
      }
    }
    if(dominoes.isEmpty) {
      Some(Nil)
    } else {
      val (g, chain) = findChain(AdjacencyMatrix.ofList(dominoes.tail), List(dominoes.head))
      var keepGoing = true
      var extended = chain
      var currGraph = g
      while(keepGoing) {
        var i = -1
        var chainFound: Seq[(Int, Int)] = Seq()
        while (i < extended.length - 1 && chainFound.size <= 1) {
          i += 1
          val insertAt = extended(i)
          val prev = currGraph
          val x = findChain(currGraph, List(insertAt))
          currGraph = x._1
          chainFound = x._2
          if (chainFound.size <= 1) {
            currGraph = prev
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
      if(extended.length == dominoes.length && extended.head._1 == extended.last._2) {
        Some(extended)
      } else {
        None
      }
    }
  }
}

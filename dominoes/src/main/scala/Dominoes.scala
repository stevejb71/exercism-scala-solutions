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

object Dominoes {
  def chain(dominoes: List[(Int, Int)]): Option[List[(Int, Int)]] = {
    if(dominoes.isEmpty) {
      Some(Nil)
    } else {
      @tailrec def findChain(g: AdjacencyMatrix, chain: List[(Int, Int)]): (AdjacencyMatrix, List[(Int, Int)]) = {
        (g.pickMatching(chain.head._1), g.pickMatching(chain.last._2)) match {
          case (Some((g1, first)), _) => findChain(g1, first.swap :: chain)
          case (_, Some((g1, last))) => findChain(g1, chain :+ last)
          case _ => (g, chain)
        }
      }
      @tailrec def findInsertionPoint(i: Int, extended: List[(Int, Int)], chainFound: Seq[(Int, Int)], currGraph: AdjacencyMatrix): (Int, Seq[(Int, Int)], AdjacencyMatrix) =  {
        if(i < extended.length && chainFound.size <= 1) {
          val (nextGraph, nextChain) = findChain(currGraph, List(extended(i)))
          findInsertionPoint(i + 1, extended, nextChain, if (nextChain.size <= 1) currGraph else nextGraph)
        } else {
          (i, chainFound, currGraph)
        }
      }
      @tailrec def extendChain(i: Int, extended: List[(Int, Int)], currGraph: AdjacencyMatrix): List[(Int, Int)] = {
        val (nextIndex, chainFound, nextGraph) = findInsertionPoint(i, extended, Seq(), currGraph)
        if (chainFound.size > 1 && extended.length < dominoes.length) {
          val (before, after) = extended.splitAt(nextIndex)
          extendChain(nextIndex, before ++ chainFound.tail ++ after, nextGraph)
        } else {
          extended
        }
      }
      val (g, chain) = findChain(dominoes.tail.foldLeft(AdjacencyMatrix()) {case (am, (x1, x2)) => am.set(x1, x2)}, List(dominoes.head))
      val longestPossibleChain = extendChain(0, chain, g)
      if(longestPossibleChain.length == dominoes.length && longestPossibleChain.head._1 == longestPossibleChain.last._2) {
        Some(longestPossibleChain)
      } else {
        None
      }
    }
  }
}

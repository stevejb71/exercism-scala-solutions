import collection.immutable.Map
import atto._
import Atto._
import compat.cats._
import cats.implicits._

sealed trait Expr {
  def eval(substs: Map[Char, Int]): Option[Int]
}
case class Lit(n: Int) extends Expr {
  def eval(substs: Map[Char, Int]): Option[Int] = n.some
}
case class Value(s: String) extends Expr {
  def eval(substs: Map[Char, Int]): Option[Int] =
    if(substs(s(0)) == 0) {
      none
    } else {
      (s.toSeq map substs.apply).mkString.toInt.some
    }
}
case class Op(lhs: Expr, rhs: Expr, f: (Int, Int) => Int) extends Expr {
  def eval(substs: Map[Char, Int]) = (lhs eval substs) |@| (rhs eval substs) map f
}

case class Equation(lhs: Expr, rhs: Expr) {
  def satisfiedBy(substs: Map[Char, Int]): Boolean =
    (lhs eval substs) |@| (rhs eval substs) map (_ == _) getOrElse false
}

object Alphametics {
  def solve(equationStr: String): Option[Map[Char, Int]] = {
    val letters = (equationStr filter (_.isLetter)).distinct.toSeq
    for {
      equation <- (equationParser parse equationStr).done.option
      solution <- solveEquation(equation, letters)
    } yield solution
  }

  private def solveEquation(e: Equation, letters: Seq[Char]): Option[Map[Char, Int]] = {
    val dummyLetter = ' '
    val withDummies = letters ++ Seq.fill(10 - letters.size)(dummyLetter)
    val perms = withDummies.permutations.map(_.zipWithIndex filter {case (ch, _) => ch != dummyLetter}).map(_.toMap)
    perms find e.satisfiedBy
  }

  private val equationParser: atto.Parser[Equation] = {
    val literal = int.map(Lit(_).asInstanceOf[Expr])
    val unknown = stringOf(charRange('A' to 'Z')).map(Value(_).asInstanceOf[Expr])
    val value = literal | unknown
    def makeParser(lhsParser: Parser[Expr], op: Char, rhsParser: Parser[Expr], f: (Int, Int) => Int): Parser[Expr] =
      ((lhsParser <~ string(s" $op ")) ~ rhsParser) map {case (lhs, rhs) => Op(lhs, rhs, f)}
    val exp = makeParser(value, '^', value, Math.pow(_, _).asInstanceOf[Int])
    lazy val mul: Parser[Expr] = makeParser(value, '*', exp | mul | value, _ * _)
    lazy val add: Parser[Expr] = makeParser(value, '+', mul | add | value, _ + _)
    val expr = exp | mul | add | value
    ((expr <~ string(" == ")) ~ expr).map{case (x,y) => Equation(x,y)}
  }
}

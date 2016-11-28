import collection.immutable._
import cats.implicits._
import scala.annotation.tailrec

object AllYourBase {
  def rebase(from: Int, digits: List[Int], target: Int): Option[List[Int]] =
    if(from <= 1 || target <= 1) {
      none
    } else {
      val asInt = digits.foldM(0) { case (acc, d) =>
        if (d < 0 || d >= from) none else (acc * from + d).some }
      asInt.map(toDigits(target))
    }

  @tailrec
  private def toDigits(base: Int, acc: List[Int] = Nil)(n: Int): List[Int] =
    n match {
      case 0 => acc
      case _ => toDigits(base, n % base :: acc)(n / base)
    }
}

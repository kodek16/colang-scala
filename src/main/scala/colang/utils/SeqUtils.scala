package colang.utils

import scala.annotation.tailrec

object SeqUtils {

  /**
    * Joins xs inserting ys between every sequence in xs.
    */
  def interleave[T](xs: Seq[Seq[T]], ys: Seq[T]): Seq[T] = {
    @tailrec
    def interleaveTail(xs: Seq[Seq[T]], accumulator: Vector[T] = Vector.empty): Vector[T] = {
      xs match {
        case _ +: _ +: _ => interleaveTail(xs.tail, accumulator ++ xs.head ++ ys)
        case _ +: _ => accumulator ++ xs.head
        case _ => Vector.empty
      }
    }

    interleaveTail(xs)
  }
}

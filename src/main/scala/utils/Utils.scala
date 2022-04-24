package utils

import calculator.PeakType.PeakType

object Utils {
  type DivergencePoints = ((Seq[(Double, Int)], Seq[(Double, Int)], PeakType), (Seq[(Double, Int)], Seq[(Double, Int)], PeakType))
  type Divergence = (Seq[(Double, Int)], Seq[(Double, Int)], PeakType)

  def sigm(d: Double) = 2 * ((1 / (1 + math.exp(-50 * d))) - 0.5)
  def greaterFromTuple2(values: (Double, Double)) = Seq(math.abs(values._1), math.abs(values._2)).max
  def avgChange(x: (Double, Double)) = (x._1 - x._2) / x._2

  case class Peak(value: Double, timeframe: Int)

  case class DirectedPeak(value: Double, timeframe: Int, peakType: PeakType)

  implicit class SafeMaxSyntax[T, A](t: T)(implicit ev: T => Vector[A], ord: Ordering[A]) {
    def safeMax: Option[A] = {
      val coll = ev(t)
      if (coll.nonEmpty) Some(coll.max) else None
    }

    def safeMin: Option[A] = {
      val coll = ev(t)
      if (coll.nonEmpty) Some(coll.min) else None
    }
  }
}

package utils

object UtilFunctions {
  def sigm(d: Double) = 2 * ((1 / (1 + math.exp(-50 * d))) - 0.5)
  def greaterFromTuple2(values: (Double, Double)) = Seq(math.abs(values._1), math.abs(values._2)).max
  def avgChange(x: (Double, Double)) = (x._1 - x._2) / x._2
}

package calculator

import utils.Utils.avgChange

class PeakFinder {
  import PeakType._

  def findPeaks(buffer: Vector[Double], timeFrame: Int, thresholds: (Double, Double)): ((Double, Int, PeakType), (Double, Int, PeakType)) = {

    val bufferSize = buffer.length
    val averageChange = buffer.slice(1, bufferSize) zip buffer.slice(0, bufferSize - 1) map avgChange
    val cumulative = averageChange.scanLeft(0.0)((x1, x2) => x1 + x2).tail
    val min = cumulative.min
    val max = cumulative.max
    val indexMin = cumulative.indexOf(min) + 1
    val indexMax = cumulative.indexOf(max) + 1

    val maxValueAfterMinimum = extremalValueAfterDetectedExtremum(indexMin, cumulative, (x: Vector[Double]) => x.max)
    val minValueAfterMaximum = extremalValueAfterDetectedExtremum(indexMax, cumulative, (x: Vector[Double]) => x.min)

    val minimumSurrounding = recoveryValue(indexMin, cumulative, (x1: Vector[Double], x2: Vector[Double]) => x2.max - x1.max)
    val maximumSurrounding = recoveryValue(indexMax, cumulative, (x1: Vector[Double], x2: Vector[Double]) => x1.min - x2.min)

    val isMinValid = min <= thresholds._1 && maxValueAfterMinimum != Double.NaN && (minimumSurrounding > -math.abs(thresholds._1) / 3 || maxValueAfterMinimum - min > math.abs(thresholds._1) / 3)
    val isMaxValid = max >= thresholds._2 && minValueAfterMaximum != Double.NaN && (maximumSurrounding > -math.abs(thresholds._2) / 3 || max - minValueAfterMaximum > math.abs(thresholds._2) / 3)
    val minimum = if (isMinValid) (buffer(indexMin), timeFrame - (bufferSize - cumulative.indexOf(min)) + 1, MIN) else (0.0, 0, MIN)
    val maximum = if (isMaxValid) (buffer(indexMax), timeFrame - (bufferSize - cumulative.indexOf(max)) + 1, MAX) else (0.0, 0, MAX)
    (minimum, maximum)
  }

  def extremalValueAfterDetectedExtremum(index: Int, cumulative: Vector[Double], extremumFunction: Vector[Double] => Double) = {
    cumulative.splitAt(index) match {
      case vec if vec._2 nonEmpty => extremumFunction(vec._2)
      case _ => Double.NaN
    }
  }

  def recoveryValue(index: Int, cumulative: Vector[Double], reduceFunction: (Vector[Double], Vector[Double]) => Double) = {
    cumulative.splitAt(index) match {
      case vec if vec._1.nonEmpty && vec._2.nonEmpty => reduceFunction(vec._1, vec._2)
      case _ => Double.NaN
    }
  }
}

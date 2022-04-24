package calculator

import calculator.PeakType.{MIN, PeakType}
import utils.Utils.{Divergence, DivergencePoints, greaterFromTuple2, sigm}



class DivergenceDetector {
  def getDivergenceRatio(points: DivergencePoints): (PeakType, Double) = {

    //todo: rozważ refaktor na jakiś partial function
    // znajdujemy takie sety, które zawierają 2 elementy - początkowy, i końcowy, dla obliczenia dywergencji
    val divergencePoints = points.productIterator.map(_.asInstanceOf[(Seq[(Double, Int)], Seq[(Double, Int)], PeakType)])
      .filter(x => x._1.size == 2 && x._2.size == 2).toVector

    // znajdujemy, któro z ekstremów jest najbliższe bieżącemu czasowi (maksymalny timeframe), i dla niego liczymy dywergencję
    def findDivergencesForRecentExtremum = (divergencePoints: Vector[Divergence]) => {
      val result = divergencePoints.max(Ordering.by[Divergence, Int](_._2.head._2))
      (result._3, findDivergences((result._1.head, result._1.last), (result._2.head, result._2.last)))
    }

    if (divergencePoints.nonEmpty) {
      findDivergencesForRecentExtremum(divergencePoints)
    } else {
      (MIN, 0.0)
    }

  }

  def findDivergences(indicatorPeaks: ((Double, Int), (Double, Int)), pricePeaks: ((Double, Int), (Double, Int))): Double = {

    def normalize(tuple: ((Double, Int), (Double, Int))) = {

      val absGreaterOfThatBothValues = greaterFromTuple2(tuple._1._1, tuple._2._1)

      ((tuple._1._1 / absGreaterOfThatBothValues, tuple._1._2), (tuple._2._1 / absGreaterOfThatBothValues, tuple._2._2))
    }

    val indicatorPeaksNorm = normalize(indicatorPeaks)
    val pricePeaksNorm = normalize(pricePeaks)

    val indicatorMonotonicity = (indicatorPeaksNorm._2._1 - indicatorPeaksNorm._1._1) / (indicatorPeaksNorm._2._2 - indicatorPeaksNorm._1._2)
    val priceMonotonicity = (pricePeaksNorm._2._1 - pricePeaksNorm._1._1) / (pricePeaksNorm._2._2 - pricePeaksNorm._1._2)

    sigm(indicatorMonotonicity - priceMonotonicity)
  }

}

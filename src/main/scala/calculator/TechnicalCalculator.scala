package calculator

import calculator.PeakType.{MAX, MIN, PeakType}
import calculator.indicators._
import utils.Utils.DivergencePoints

class TechnicalCalculator {
  val RSI: RSI = new RSI(15)
  val MA: EMA = new EMA(20)
  val OBV: OBV = new OBV
  val support: Support = new Support(7)
  val resistance: Resistance = new Resistance(7)

  var RSIBuffer: Vector[Double] = Vector()
  var MABuffer: Vector[Double] = Vector() //?
  var OBVBuffer: Vector[Double] = Vector()
  var SupportBuffer: Vector[Double] = Vector() //?
  var ResistanceBuffer: Vector[Double] = Vector() //?
  var ClosePriceBuffer: Vector[Double] = Vector()

  var RSIpeaks: Set[(Double, Int, PeakType)] = Set()
  var MApeaks: Set[(Double, Int, PeakType)] = Set()
  var OBVpeaks: Set[(Double, Int, PeakType)] = Set()
  var SupportPeaks: Set[(Double, Double, Int)] = Set() //?
  var ResistancePeaks: Set[(Double, Double, Int)] = Set() //?
  var ClosePricePeaks: Set[(Double, Int, PeakType)] = Set()
  var timeframe: Int = 0
  var latestRecommendation = 0.0

  val peakFinder: PeakFinder = new PeakFinder
  val divergenceDetector: DivergenceDetector = new DivergenceDetector

  /**
   *
   * @param data (Close, Volume, Open, High, Low)
   */
  def initialize(data: Vector[(Double, Double, Double, Double, Double)]): Double = {
    if (data.length == 20) {
      val close = data map (x => (x._1))
      val close_volume = data map (x => (x._1, x._2))
      val all_price_params = data map (x => (x._1, x._3, x._4, x._5))

      val rsi = RSI.initialize(close slice(0, 15))
      val ma = MA.initialize(close)
      val sup = support.initialize(all_price_params slice(0, 7))
      val res = resistance.initialize(all_price_params slice(0, 7))
      val obv = OBV.initialize(close_volume slice(0, 15))

      RSIBuffer = (for (i <- 14 until 20) yield RSI.update(close(i))).toVector
      OBVBuffer = (for (i <- 14 until 20) yield OBV.update(close_volume(i))).toVector
      ClosePriceBuffer = close.slice(14, 20)

      OBVpeaks = OBVpeaks ++ getPeaksFor(OBVBuffer, 20, (-0.03,0.03)) //? dawaj je z przodu
      RSIpeaks = RSIpeaks ++ getPeaksFor(RSIBuffer, 20, (-0.1, 0.1))
      ClosePricePeaks = ClosePricePeaks ++ getPeaksFor(ClosePriceBuffer, 20, (-0.03, 0.03))
      SupportPeaks = SupportPeaks ++ Vector((sup._1, sup._2, timeframe))
      ResistancePeaks = ResistancePeaks ++ Vector((res._1, res._2, timeframe))

      latestRecommendation = getRecommendation(OBVpeaks.toSeq.sortWith(_._2 > _._2), RSIpeaks.toSeq.sortWith(_._2 > _._2), SupportPeaks.toSeq.sortWith(_._3 > _._3), ResistancePeaks.toSeq.sortWith(_._3 > _._3))
      timeframe = 20
    }
    latestRecommendation
  }

  private def twoFirst(v: Iterable[(Double, Int, PeakType)], t: PeakType): Seq[(Double, Int)] = (v.view).filter(_._3 == t).map(x => (x._1, x._2)).take(2).force.toSeq

  def update(data: (Double, Double, Double, Double, Double)): Double = {
    val rsi = RSI.update(data._1)
    val ma = MA.update(data._1)
    val sup = support.update(data._1, data._3, data._4, data._5)
    val res = resistance.update(data._1, data._3, data._4, data._5)
    val obv = OBV.update(data._1, data._2)
    timeframe = timeframe + 1

    RSIBuffer = RSIBuffer.slice(1, RSIBuffer.size) ++ Vector(rsi)
    OBVBuffer = OBVBuffer.slice(1, OBVBuffer.size) ++ Vector(obv)
    ClosePriceBuffer = ClosePriceBuffer.slice(1, ClosePriceBuffer.size) ++ Vector(data._1)

    if (timeframe % 4 == 0) {
      OBVpeaks = (OBVpeaks ++ getPeaksFor(OBVBuffer, timeframe, (-0.03, 0.03))).filter(x => x._2 != 0)
      RSIpeaks = (RSIpeaks ++ getPeaksFor(RSIBuffer, timeframe, (-0.1, 0.1))).filter(x => x._2 != 0)
      ClosePricePeaks = (ClosePricePeaks ++ getPeaksFor(ClosePriceBuffer, timeframe, (-0.03, 0.03))).filter(x => x._2 != 0) //todo: co jeśli jest pik na OBV, a nie ma na close price
      SupportPeaks = SupportPeaks ++ Vector((sup._1, sup._2, timeframe))
      ResistancePeaks = ResistancePeaks ++ Vector((res._1, res._2, timeframe))
      latestRecommendation = getRecommendation(OBVpeaks.toSeq.sortWith(_._2 > _._2), RSIpeaks.toSeq.sortWith(_._2 > _._2), SupportPeaks.toSeq.sortWith(_._3 > _._3), ResistancePeaks.toSeq.sortWith(_._3 > _._3))
    }

    latestRecommendation
  }

  private def getRecommendation(OBVpeaks: Seq[(Double, Int, PeakType)], RSIpeaks: Seq[(Double, Int, PeakType)], sup: Seq[(Double, Double, Int)], res: Seq[(Double, Double, Int)]): Double = {
    def sigm = (d: Double) => 1 / (1 + math.exp(-d))

    val closePriceAndOBV = (
      (getClosePricePeaksFor(twoFirst(OBVpeaks, MAX)), twoFirst(OBVpeaks, MAX), PeakType.MAX),
      (getClosePricePeaksFor(twoFirst(OBVpeaks, MIN)), twoFirst(OBVpeaks, MIN), PeakType.MIN)
    )

    val closePriceAndRSI = (
      (getClosePricePeaksFor(twoFirst(RSIpeaks, MAX)), twoFirst(RSIpeaks, MAX), PeakType.MAX),
      (getClosePricePeaksFor(twoFirst(RSIpeaks, MIN)), twoFirst(RSIpeaks, MIN), PeakType.MIN)
    )

    0.4 * sigm(getOBVRatio(closePriceAndOBV)._2) + 0.4 * sigm(getRSIRatio(closePriceAndRSI, RSIpeaks)) + 0.2 * sigm(getSupportResistanceRatio(sup, res, ClosePriceBuffer))
  }

  private def getClosePricePeaksFor(timeframes: Seq[(Double, Int)]) = {
    ClosePricePeaks.filter(x => timeframes.map(_._2).contains(x._2)).map(x => (x._1, x._2)).toSeq
  }

  def findDivergences(indicatorPeaks: ((Double, Int), (Double, Int)), pricePeaks: ((Double, Int), (Double, Int))): Double = {
    divergenceDetector.findDivergences(indicatorPeaks, pricePeaks)
  }

  private def getOBVRatio(closePriceAndOBV: DivergencePoints) = {
    divergenceDetector.getDivergenceRatio(closePriceAndOBV)
  }

  private def getRSIRatio(closePriceAndRSI: DivergencePoints, RSIpeaks: Seq[(Double, Int, PeakType)]) = {
    divergenceDetector.getDivergenceRatio(closePriceAndRSI)._2 + analyzeRsiTrend(RSIpeaks)
  }

  private def getSupportResistanceRatio(support: Seq[(Double, Double, Int)], resistance: Seq[(Double, Double, Int)], priceBuffer: Seq[Double]) = {
    val averageMonotonicity = (priceBuffer.slice(1, priceBuffer.length) zip priceBuffer.slice(0, priceBuffer.length - 1) map (x => x._1 - x._2) sum) / priceBuffer.length
    val validSupport = support.find(_._2 >= 0.8).getOrElse((0.0, 0.0, 0))
    val validResistance = resistance.find(_._2 >= 0.8).getOrElse((0.0, 0.0, 0))
    val closePrice = priceBuffer.reverse.headOption.getOrElse(0.0)
    if (averageMonotonicity < 0) {
      val similarSupport = support.count(x => math.abs(x._1 - validSupport._1) / x._1 < 0.25)
      similarSupport / (0.5 * (validSupport._1 - closePrice))
    } else {
      val similarResistance = resistance.count(x => math.abs(x._1 - validResistance._1) / x._1 < 0.25)
      similarResistance / (0.5 * (closePrice - validResistance._1))
    }
  }

  // https://www.newtraderu.com/2020/07/11/rsi-divergence-cheat-sheet/
  def analyzeRsiTrend(rsiPeaks: Iterable[(Double, Int, PeakType)]): Double = {
    def boost(rsi: Double): Double = math.pow((rsi - 50) / 5, 3) / 2 + math.pow(((rsi - 50) / 20), 3)

    def sigm = (d: Double) => 2 * ((1 / (1 + math.exp(-0.01 * d))) - 0.5)

    if (rsiPeaks.size > 3) {
      sigm(Seq(25.0, 5.0, 1.0).toVector.map(_ / 2).zip(rsiPeaks.take(3).map(x => boost(x._1))).map(x => x._1 * x._2).sum)
    } else {
      0.0
    }
  }

  def getPeaksFor(buffer: Vector[Double], timeFrame: Int, thresholds: (Double, Double)): Seq[(Double, Int, PeakType)] = {
    val extrema = findPeaks(buffer, timeFrame, thresholds)
    Seq(extrema._1, extrema._2)
  }

  def findPeaks(buffer: Vector[Double], timeFrame: Int, thresholds: (Double, Double)) = {
    peakFinder.findPeaks(buffer, timeFrame, thresholds)
  }

  //todo: więcej testów
  //todo: jeśli badamy minima, to jedna z funkcji "napłask" oznacza potencjalny wzrost
  //todo: malejąca cena, rosnący wskaźnik - bearish, czyli spadek
  // https://therobusttrader.com/how-to-trade-the-obv-indicator-complete-obv-guide-on-balance-volume/

  // thresholds - poziom zmiany, jaki uznajemy za pik, oraz minimalna wartość do jakiej cena musi wrócić, żeby ektremum było uznane za pik

}

package calculator

import calculator.PeakType.{MAX, MIN, PeakType}
import calculator.indicators._

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
  var MApeaks: Set[(Double, Int, PeakType)] = Set() //?
  var OBVpeaks: Set[(Double, Int, PeakType)] = Set()
  var SupportPeaks: Set[(Double, Int, PeakType)] = Set() //?
  var ResistancePeaks: Set[(Double, Int, PeakType)] = Set() //?
  var ClosePricePeaks: Set[(Double, Int, PeakType)] = Set()
  var timeframe: Int = 0
  var latestRecommendation = 0.0

  /**
   *
   * @param data (Close, Volume, Open, High, Low)
   */
  def initialize(data: Vector[(Double, Double, Double, Double, Double)]): Double = {
    if(data.length == 20) {
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

      OBVpeaks = getPeaksForOBV(20) //? dawaj je z przodu
      RSIpeaks = getPeaksForRSI(20)
      ClosePricePeaks = getPeaksForClose(20)

      latestRecommendation = getRecommendation(OBVpeaks, RSIpeaks, sup, res)
    }
    latestRecommendation
  }

  private def toTuple2(col: Iterable[(Double, Int)]) = (col.head, col.last)
  private def twoFirst(v: Set[(Double, Int, PeakType)], t: PeakType): Set[(Double, Int)] = (v.view).filter(_._3 == t).map(x => (x._1, x._2)).take(2).force.toSet

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

    if(timeframe % 4 == 0) {
      OBVpeaks = (getPeaksForOBV(timeframe) ++ OBVpeaks).filter(x => x._2 == 0)
      RSIpeaks = (getPeaksForRSI(timeframe) ++ RSIpeaks).filter(x => x._2 == 0)
      ClosePricePeaks = (getPeaksForClose(timeframe) ++ ClosePricePeaks).filter(x => x._2 == 0) //todo: co jeśli jest pik na OBV, a nie ma na close price

      latestRecommendation = getRecommendation(OBVpeaks, RSIpeaks, sup, res)
    }

    latestRecommendation
  }

  private def getRecommendation(OBVpeaks: Set[(Double, Int, PeakType)], RSIpeaks: Set[(Double, Int, PeakType)], sup: (Double, Double), res: (Double, Double)): Double = {
    def sigm = (d: Double) => 1/1 + math.exp(-d)
    val closePriceAndOBV = (
      (getClosePricePeaksFor(twoFirst(OBVpeaks, MAX)), twoFirst(OBVpeaks, MAX), PeakType.MAX),
      (getClosePricePeaksFor(twoFirst(OBVpeaks, MIN)), twoFirst(OBVpeaks, MIN), PeakType.MIN)
    )

    val closePriceAndRSI = (
      (getClosePricePeaksFor(twoFirst(RSIpeaks, MAX)), twoFirst(RSIpeaks, MAX), PeakType.MAX),
      (getClosePricePeaksFor(twoFirst(RSIpeaks, MIN)), twoFirst(RSIpeaks, MIN), PeakType.MIN)
    )

    0.5*sigm(getOBVRatio(closePriceAndOBV)._2) + 0.5*sigm(getRSIRatio(closePriceAndRSI, RSIpeaks))// + sup._1*sup._2 + res._1*res._2
  }

  private def getClosePricePeaksFor(timeframes: Set[(Double, Int)]) = {
    ClosePricePeaks.filter(x => timeframes.map(_._2).contains(x._2)).map(x => (x._1, x._2))
  }

  type DivergencePoints = ((Set[(Double, Int)], Set[(Double, Int)], PeakType), (Set[(Double, Int)], Set[(Double, Int)], PeakType))

  private def getDivergenceRatio(points: DivergencePoints): (PeakType, Double) ={
    val mostRecent = points.productIterator.map(_.asInstanceOf[(Set[(Double, Int)], Set[(Double, Int)], PeakType)])
      .filter(x => x._1.size == 2 && x._2.size == 2)
      .max(Ordering.by[(Set[(Double, Int)], Set[(Double, Int)], PeakType), Int](_._2.head._2))

    (mostRecent._3, findDivergences((mostRecent._1.head, mostRecent._1.last), (mostRecent._2.head, mostRecent._2.last)))

  }

  def getOBVRatio(closePriceAndOBV: DivergencePoints) = {
    getDivergenceRatio(closePriceAndOBV)
  }

  def getRSIRatio(closePriceAndRSI: DivergencePoints, RSIpeaks: Set[(Double, Int, PeakType)]) = {
    getDivergenceRatio(closePriceAndRSI)
    analyzeRsiTrend(RSIpeaks)
  }

  //todo: możliwe przypadki, uwzględnienie ostatniego punktu ze szczególnością
  // https://www.newtraderu.com/2020/07/11/rsi-divergence-cheat-sheet/
  def analyzeRsiTrend(rsiPeaks: Iterable[(Double, Int, PeakType)]): Double = {
    def boost(rsi: Double): Double = math.pow((rsi-50)/5, 3)/2 + math.pow((rsi-50)%20, 3)
    if(rsiPeaks.size == 3){
      (1 to 3).toVector.map(_/2).zip(rsiPeaks.map(x => boost(x._1))).map(x => x._1*x._2)
    }
    0.0
  }

  def getPeaksForOBV(timeFrame: Int): Set[(Double, Int, PeakType)] = {
    val extrema = findPeaks(OBVBuffer, timeFrame, (-0.03, 0.03))
    Set(extrema._1, extrema._2)
  }

  def getPeaksForRSI(timeFrame: Int): Set[(Double, Int, PeakType)] = {
    val extrema = findPeaks(RSIBuffer, timeFrame, (-0.2, 0.2))
    Set(extrema._1, extrema._2)
  }

  def getPeaksForClose(timeFrame: Int): Set[(Double, Int, PeakType)] = {
    val extrema = findPeaks(ClosePriceBuffer, timeFrame, (-0.03, 0.03))
    Set(extrema._1, extrema._2)
  }

  //todo: więcej testów
  //todo: jeśli badamy minima, to jedna z funkcji "napłask" oznacza potencjalny wzrost
  //todo: malejąca cena, rosnący wskaźnik - bearish, czyli spadek
  // https://therobusttrader.com/how-to-trade-the-obv-indicator-complete-obv-guide-on-balance-volume/
  def findDivergences(indicatorPeaks: ((Double, Int), (Double, Int)), pricePeaks: ((Double, Int), (Double, Int))): Double = {
    val normalize = (tuple: ((Double, Int), (Double, Int))) => toTuple2(tuple.productIterator.map(x => (x.asInstanceOf[(Double, Int)])).map(x => (x._1 / tuple.productIterator.map(x => (x.asInstanceOf[(Double, Int)])).max(Ordering.by[(Double, Int), Double](_._1)).asInstanceOf[Double], x._2)).toVector)

    val indicatorPeaksNorm = normalize(indicatorPeaks)
    val pricePeaksNorm = normalize(pricePeaks)

    val indicatorMonotonicity = (indicatorPeaksNorm._2._1 - indicatorPeaksNorm._1._1)/(indicatorPeaksNorm._2._2 - indicatorPeaksNorm._1._2)
    val priceMonotonicity = (pricePeaksNorm._2._1 - pricePeaksNorm._1._1)/(pricePeaksNorm._2._2 - pricePeaksNorm._1._2)

    indicatorMonotonicity - priceMonotonicity
  }

  // thresholds - poziom zmiany, jaki uznajemy za pik, oraz minimalna wartość do jakiej cena musi wrócić, żeby ektremum było uznane za pik
  import PeakType._
  def findPeaks(buffer: Vector[Double], timeFrame: Int, thresholds: (Double, Double)): ((Double, Int, PeakType), (Double, Int, PeakType)) = {
    def avgChange(x: (Double, Double)) = (x._1 - x._2) / x._2
    val bufferSize = buffer.length
    val averageChange = buffer.slice(1, bufferSize) zip buffer.slice(0, bufferSize - 1) map avgChange
    val cumulative = averageChange.scanLeft(0.0)((x1, x2) => x1 + x2).tail
    val min = cumulative.min
    val max = cumulative.max
    val indexMin = cumulative.indexOf(min) + 1
    val indexMax = cumulative.indexOf(max) + 1

    val minimumSurround = if (indexMin < cumulative.size-1) cumulative.splitAt(indexMin).productIterator.filter(x => x.asInstanceOf[Vector[Double]].nonEmpty).map(x => x.asInstanceOf[Vector[Double]].max).toVector else Vector()
    val maximumSurround = if (indexMax < cumulative.size-1) cumulative.splitAt(indexMax).productIterator.filter(x => x.asInstanceOf[Vector[Double]].nonEmpty).map(x => x.asInstanceOf[Vector[Double]].min).toVector else Vector()

    val minimumSurrounding = if (indexMin > 0) cumulative.splitAt(indexMin).productIterator.map(x => x.asInstanceOf[Vector[Double]].max).reduce((x1, x2) => x2 - x1) else -math.abs(thresholds._1)
    val maximumSurrounding = if (indexMax > 0) cumulative.splitAt(indexMax).productIterator.map(x => x.asInstanceOf[Vector[Double]].min).reduce((x1, x2) => x1 - x2) else -math.abs(thresholds._2)
    // czy po osiągnięciu ekstremum cena wróciła w pobliże początkowej - cumulative czy averageChange??
    val isMinValid = min <= thresholds._1 && (minimumSurrounding > -math.abs(thresholds._1)/3 || minimumSurround.reverse.head - min > math.abs(thresholds._1)/3)
//    val isMinValid = min <= thresholds._1 && (cumulative.slice(indexMin+1, cumulative.size) count (math.abs(_) <= math.abs(thresholds._1))) > 0
    val isMaxValid = max >= thresholds._2 && (maximumSurrounding > -math.abs(thresholds._2)/3 || max - maximumSurround.reverse.head > math.abs(thresholds._2)/3)
//    val isMaxValid = max >= thresholds._2 && (cumulative.slice(indexMax+1, cumulative.size) count (math.abs(_) <= math.abs(thresholds._2))) > 0
    val minimum = if (isMinValid)(buffer(indexMin), timeFrame - (bufferSize - cumulative.indexOf(min)) + 1, MIN) else (0.0,0, MIN)
    val maximum = if (isMaxValid)(buffer(indexMax), timeFrame - (bufferSize - cumulative.indexOf(max)) + 1, MAX) else (0.0,0, MAX)
    (minimum, maximum)
  }
}

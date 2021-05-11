import calculator.PeakType
import org.scalatest.prop.TableDrivenPropertyChecks

object TestCases extends TableDrivenPropertyChecks {
  private val sampleList1 = Vector(1, 2, 3, 4, 5, 6, 7, 8, 7, 6, 5, 4, 3, 2, 1).map(_.toDouble)
  private val sampleList2 = Vector(8, 7, 8, 7, 6, 7, 9, 8, 6, 5, 6, 8, 7, 5, 4).map(_.toDouble)
  private val sampleList3 = Vector(2, 3, 4, 3, 2, 1, 1, 2, 3, 5, 2, 3, 2, 5, 7).map(_.toDouble)
  private val obv = Vector(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1).map(_.toDouble)

  private val supportPresent = Vector(5.52, 5.15, 5.44, 5.18, 5.0, 4.85, 5.11)
  private val supportAbsent = Vector(5.885, 5.312, 4.832, 5.018, 4.771, 4.523, 4.211)
  private val resistanceAbsent = Vector(4.4, 4.6, 5.1, 4.7, 5.3, 5.75, 6.03)
  private val resistancePresent = Vector(5.11, 5.38, 5.51, 5.45, 5.22, 5.37, 5.35)

  private val peakMin = Vector(10, 8, 6, 6, 7, 9, 10).map(_.toDouble)
  private val peakMax = Vector(10, 12, 14, 12, 11, 10, 10).map(_.toDouble)
  private val noPeak = Vector(10, 11, 11, 10, 11, 10, 10).map(_.toDouble)

  private val sampleTechCalculatorInit = Vector(
    (15.74, 1534336.0, 15.2, 15.77, 15.05),
    (14.945, 1542672.0, 15.1, 15.39, 14.69),
    (15.04, 3446288.0, 15.0, 15.95, 14.89),
    (14.31, 1174865.0, 14.37, 14.52, 14.1),
    (14.64, 1445931.0, 14.43, 14.675, 13.805),
    (15.05, 1330991.0, 15.3, 15.55, 15.01),
    (15.33, 1794892.0, 15.25, 15.5, 15.18),
    (15.265, 1047071.0, 15.36, 15.425, 14.99),
    (15.48, 1562304.0, 15.525, 15.56, 14.94),
    (15.6, 2534752.0, 15.7379, 16.1001, 15.38),
    (15.49, 3161988.0, 15.75, 16.18, 15.44),
    (15.9, 1254546.0, 16.25, 16.35, 15.58),
    (16.11, 2430602.0, 16.739, 16.739, 15.58),
    (16.46, 3613208.0, 17.87, 18.17, 16.42),
    (18.07, 8381803.0, 16.64, 19.17, 16.63),
    (16.25, 3813766.0, 13.72, 16.435, 13.7),
    (14.02, 4560094.0, 14.47, 14.959, 13.91),
    (15.14, 4141350.0, 15.47, 15.8, 14.93),
    (15.14, 3772161.0, 14.05, 15.58, 13.87),
    (14.0, 2103511.0, 14.06, 14.2493, 13.63)
    //(13.98,2001152.0,14.06,14.33,13.6)
  )

  private val sampleTechnicalCalculatorUpdate = Vector(
    (13.98, 2001152.0, 14.06, 14.33, 13.6),
    (14.03, 4781439.0, 15.05, 15.91, 13.87),
    (15.08, 9347702.0, 13.73, 15.765, 13.5),
    (13.865, 7636864.0, 12.79, 14.02, 12.75)
  )

  val RSIInitCases = Table(
    "RSI init",
    (sampleList1, 50.0),
    (sampleList2, 38.88),
    (sampleList3, 63.16),
  )

  val RSIUpdateCases = Table(
    "RSI update",
    (sampleList1, 3.0, 56.66),
    (sampleList2, 2.0, 34.73),
    (sampleList3, 9.0, 66.91),
  )

  val OBVInitCases = Table(
    "OBV init",
    (sampleList1 zip obv, 0.0),
    (sampleList2 zip obv, -4.0),
    (sampleList3 zip obv, 3.0)
  )

  val OBVUpdateCases = Table(
    "OBV update",
    (sampleList1 zip obv, 3.0, 1.0, 1.0),
    (sampleList2 zip obv, 2.0, 1.0, -5.0),
    (sampleList3 zip obv, 9.0, 1.0, 4.0)
  )

  val SupportAbsentInitCases = Table(
    "Support absent init",
    (supportAbsent map (x => (x, x, x, x)), 0.7),
    (supportAbsent map (x => x * 10) map (x => (x, x, x, x)), 0.7),
    (supportAbsent map (x => x * 100) map (x => (x, x, x, x)), 0.8),
  )

  val ResistanceAbsentInitCases = Table(
    "Resistance absent init",
    (resistanceAbsent map (x => (x, x, x, x)), 0.7),
    (resistanceAbsent map (x => x * 10) map (x => (x, x, x, x)), 0.7),
    (resistanceAbsent map (x => x * 100) map (x => (x, x, x, x)), 0.8)
  )

  //pododawaj, gdzie powinien być tn supprt, oraz resistance
  val SupportPresentInitCases = Table(
    "Support present init",
    (supportPresent map (x => (x, x, x, x)), 0.85, 5.0),
    (supportPresent map (x => x * 10) map (x => (x, x, x, x)), 0.85, 50.0),
    (supportPresent map (x => x * 100) map (x => (x, x, x, x)), 0.85, 515.0),
  )

  val ResistancePresentInitCases = Table(
    "Resistance present init",
    (resistancePresent map (x => (x, x, x, x)), 0.85, 5.5),
    (resistancePresent map (x => x * 10) map (x => (x, x, x, x)), 0.85, 55.0),
    (resistancePresent map (x => x * 100) map (x => (x, x, x, x)), 0.85, 540.0)
  )

  val PeaksCases = Table(
    "Peaks",
    (peakMin, 7, (-0.2, 0.2), ((6.0, 3, PeakType.MIN), (0.0, 0, PeakType.MAX))),
    (peakMax, 7, (-0.2, 0.2), ((0.0, 0, PeakType.MIN), (14.0, 3, PeakType.MAX))),
    (noPeak, 7, (-0.2, 0.2), ((0.0, 0, PeakType.MIN), (0.0, 0, PeakType.MAX)))
  )

  val DivergencesCases = Table(
    "Divergences",
    (((5.0, 2), (6.0, 18)), ((38.0, 2), (56.0, 18)), 0.25),
    (((5.0, 2), (6.0, 18)), ((38.0, 2), (39.0, 18)), 0.48),
    (((5.0, 2), (6.0, 18)), ((38.0, 2), (31.0, 18)), 0.9)
  )

  val TechnicalCalculatorInitializeCases = Table(
    "TechnicalCalculatorInitialize",
    (sampleTechCalculatorInit, 6, 1.0),
    (sampleTechCalculatorInit.take(10), 0, 0.0)
  )

  val TechnicalCalculatorUpdateCases = Table(
    "TechnicalCalculatorUpdate",
    (sampleTechCalculatorInit, sampleTechnicalCalculatorUpdate),
  )


}

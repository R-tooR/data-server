import calculator.TechnicalCalculator
import data.extraction.FileReader
import data.processing.DataProcessor

class Stock(file: String) {
  val reader: FileReader = new FileReader(file)
  val processor: DataProcessor = new DataProcessor
  val calculator: TechnicalCalculator = new TechnicalCalculator
  var end = false;
  var prev = 0.0
  def initialize(): Unit = {
    val initData = processor.getBatch(reader.readLines(21).slice(1, 21))
    val initRecommendation = calculator.initialize(initData)
//    println("For " + file + " base recommendation is " + initRecommendation)
  }

  def update(): Unit = {
    val line = reader.readLine.apply()
    if (line != "") {
      val newData = processor.getLine(line)
      val newRecommendation = calculator.update(newData)
      if(newRecommendation != prev) {
//        println("Time: " + calculator.timeframe + ". For " + file + " updated recommendation is " + newRecommendation)
        prev = newRecommendation
      }
    } else {
//      println("Recommendation for " + file + " finished!")
      end = true
    }
  }

  def getStockScore: Double = calculator.latestRecommendation

  def getCurrentClose: Double = calculator.ClosePriceBuffer.reverse.head
}

import calculator.TechnicalCalculator
import data.extraction.FileReader
import data.processing.DataProcessor

object App {
  def main(args: Array[String]): Unit = {
    val t = new TechnicalCalculator

//    val f = new FileReader("angi.csv")
//    val d = new DataProcessor()
//    println(f.readLine)
//    val batch = f.readLines(21).slice(1, 21)
//    println("--------")
//    println(batch)
//    val vec = d.getBatch(batch)
//    println("--------")
//    println(d.getBatch(batch))
//
//    println(t.initialize(vec))
//    println(t.update(d.getLine(f.readLine())))
    val prices = Vector(3.78848, 3.78888, 3.789, 3.7893, 3.78913, 3.78918, 3.78797, 3.79035, 3.78719, 3.78823, 3.79088, 3.78951, 3.79281, 3.7902)

//    val rsi = new t.RSI(14)
//    val rsiResult = rsi.initialize(prices)
//    val eps = 0.05
//    println("Rsi is equal to: " + rsiResult)
//    rsiResult > 54.73 - eps && rsiResult < 54.73 + eps
  }
}

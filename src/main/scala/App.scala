//import akka.http.scaladsl.model.HttpMethods.GET
//import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage}
//import akka.http.scaladsl.model.{AttributeKeys, HttpRequest, HttpResponse, Uri}
//import akka.http.scaladsl.server.Directives.{handleWebSocketMessages, path}
//import akka.stream.javadsl.{Flow, Sink, Source}
//import calculator.TechnicalCalculator
//import data.extraction.FileReader
//import data.processing.DataProcessor

import java.util
import scala.collection.parallel._
import scala.collection.parallel.mutable.ParHashMap

object App {
  val stocks = mutable.ParHashMap(
    "angi" -> new Stock("angi.csv"),
    "batra" -> new Stock("batra.csv"),
    "foxa" -> new Stock("foxa.csv"),
    "fwona" -> new Stock("fwona.csv"),
    "salm" -> new Stock("salm.csv")
  )

  private var initialized = false

//  def step(): ParHashMap[String, Double] = {
//    if (!initialized) {
//      stocks foreach (x => x._2.initialize())
//    } else {
//      stocks foreach (x => x._2.update())
//    }
//      val st = stocks.map(x => (x._1, x._2.getStockRecommendation()))
//
//  }

  def main(args: Array[String]): Unit = {
    //    val t = new TechnicalCalculator
    //
    //    val stocks = mutable.ParHashMap(
    //      "angi" -> new Stock("angi.csv"),
    //      "batra" -> new Stock("batra.csv"),
    //      "foxa" -> new Stock("foxa.csv"),
    //      "fwona" -> new Stock("fwona.csv"),
    //      "salm" -> new Stock("salm.csv")
    //    )
    ////    val pc = mutable.ParArray(1, 2, 3)
    //    val forkJoinPool = new java.util.concurrent.ForkJoinPool(2)
    //    stocks.tasksupport = new ForkJoinTaskSupport(forkJoinPool)
    ////    pc map { _ + 1 }
    //
    //    stocks foreach(x => x._2.initialize())
    //
    //    while(true) {
    //      stocks.map(x => {x._2.update()
    //        x._2.getStockRecommendation()
    //      })
    ////      Thread.sleep(2000)
    //    }


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
    //    val prices = Vector(3.78848, 3.78888, 3.789, 3.7893, 3.78913, 3.78918, 3.78797, 3.79035, 3.78719, 3.78823, 3.79088, 3.78951, 3.79281, 3.7902)

    //    val rsi = new t.RSI(14)
    //    val rsiResult = rsi.initialize(prices)
    //    val eps = 0.05
    //    println("Rsi is equal to: " + rsiResult)
    //    rsiResult > 54.73 - eps && rsiResult < 54.73 + eps
  }
}

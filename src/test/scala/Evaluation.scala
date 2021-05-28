import EvaluationCases.EvaluationRecord
import org.scalatest.Inspectors.forEvery
import org.scalatest.matchers.should
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec

import scala.collection.mutable

class Evaluation extends AnyPropSpec with should.Matchers with TableDrivenPropertyChecks  {

  private class Comparator(delta: Double, priceForecastRange: Int, onlySpecificStep: Int = 0) {
    assert(priceForecastRange > onlySpecificStep && onlySpecificStep >= 0)

    def compare(rec: Double, index: Int, closes: Vector[Double]): Boolean = {
      def priceMovementPredicted(rec: Double) = {
        if (rec > 0.5 + delta) {
          (d: Double) => d > 0
        } else if (rec < 0.5 - delta) {
          (d: Double) => d < 0
        } else {
          (d: Double) => math.abs(d) < d/10
        }
      }

      implicit class SafeTailCollection[T, A](t: T)(implicit ev: T => Vector[A], ord: Ordering[A]) {
        def tailOption: Option[Vector[A]] = {
          val coll = ev(t)
          if (coll.nonEmpty && coll.tail.nonEmpty) Some(coll.tail) else None
        }
      }

      val nextCloses = closes.splitAt(index-1)._2.take(priceForecastRange)
      val differences = nextCloses.tailOption.getOrElse(Vector()).map(cl => nextCloses.head - cl).map(dif => priceMovementPredicted(rec)(dif))

      getEvaluation(differences)
    }

    private def getEvaluation(coll: Vector[Boolean]): Boolean = {
      if(onlySpecificStep < 1)
        coll.contains(true)
      else {
        if(onlySpecificStep - 1 < coll.size) coll(onlySpecificStep-1) else false
      }
    }

  }


  def evaluation(record: EvaluationRecord) = {
    var recommendations = Vector[Double]()
    var close = Vector[Double]()
    val stock = new Stock(record.name)
    val cp = new Comparator(record.delta, record.priceForecastRange, record.onlySpecificStep)
    stock.initialize()
    while (!stock.end) {
      stock.update()
      recommendations = recommendations :+ stock.getStockScore
      close = close :+ stock.getCurrentClose
    }

    recommendations = (recommendations.indices by 4).toVector.map(x => recommendations(x))
    close = (close.indices by 4).toVector.map(x => close(x))
    val verification = recommendations.indices.toVector zip recommendations map(x => cp.compare(x._2, x._1, close)) take(recommendations.size - record.priceForecastRange)
    val ratio = verification.count(x => x).toDouble/verification.size
    println("For " + record.name + " ratio is " + ratio)

    ratio
  }

  property("Evaluation of all within three steps") {
    var result = Vector[Double]()
    forEvery(EvaluationCases.oneOfThreePeriodsAhead) { input =>
      result = result :+ evaluation(input)
    }
    println("Average is: " + result.sum/result.size)
    println("Positive correlation observed in " + (result.count(_ > 0.5).toDouble/result.size)*100 + "% of cases")
  }

  property("Evaluation of all in first step") {
    var result = Vector[Double]()
    forEvery(EvaluationCases.firstPeriodAhead) { input =>
      result = result :+ evaluation(input)
    }
    println("Average is: " + result.sum/result.size)
    println("Positive correlation observed in " + (result.count(_ > 0.5).toDouble/result.size)*100 + "% of cases")
  }

  property("Evaluation of all in second step") {
    var result = Vector[Double]()
    forEvery(EvaluationCases.secondPeriodAhead) { input =>
      result = result :+ evaluation(input)
    }
    println("Average is: " + result.sum/result.size)
    println("Positive correlation observed in " + (result.count(_ > 0.5).toDouble/result.size)*100 + "% of cases")
  }

  property("Evaluation of all in third step") {
    var result = Vector[Double]()
    forEvery(EvaluationCases.thirdPeriodAhead) { input =>
      result = result :+ evaluation(input)
    }
    println("Average is: " + result.sum/result.size)
    println("Positive correlation observed in " + (result.count(_ > 0.5).toDouble/result.size)*100 + "% of cases")
  }

  property("Evaluation of all within three steps smaller delta") {
    var result = Vector[Double]()
    forEvery(EvaluationCases.oneOfThreePeriodsAheadSmallerDelta) { input =>
      result = result :+ evaluation(input)
    }
    println("Average is: " + result.sum/result.size)
    println("Positive correlation observed in " + (result.count(_ > 0.5).toDouble/result.size)*100 + "% of cases")
  }

  property("Evaluation of all in first step smaller delta") {
    var result = Vector[Double]()
    forEvery(EvaluationCases.firstPeriodAheadSmallerDelta) { input =>
      result = result :+ evaluation(input)
    }
    println("Average is: " + result.sum/result.size)
    println("Positive correlation observed in " + (result.count(_ > 0.5).toDouble/result.size)*100 + "% of cases")
  }

  property("Evaluation of all in second step smaller delta") {
    var result = Vector[Double]()
    forEvery(EvaluationCases.secondPeriodAheadSmallerDelta) { input =>
      result = result :+ evaluation(input)
    }
    println("Average is: " + result.sum/result.size)
    println("Positive correlation observed in " + (result.count(_ > 0.5).toDouble/result.size)*100 + "% of cases")
  }

  property("Evaluation of all in third step smaller delta") {
    var result = Vector[Double]()
    forEvery(EvaluationCases.thirdPeriodAheadSmallerDelta) { input =>
      result = result :+ evaluation(input)
    }
    println("Average is: " + result.sum/result.size)
    println("Positive correlation observed in " + (result.count(_ > 0.5).toDouble/result.size)*100 + "% of cases")
  }
}
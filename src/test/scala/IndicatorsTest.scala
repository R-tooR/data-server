import TestCases.{sampleList1, sampleList2, sampleList3}
import calculator.TechnicalCalculator
import calculator.indicators._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec

//http://doc.scalatest.org/3.0.1-2.12/org/scalatest/Inspectors.html
//http://doc.scalatest.org/3.0.1-2.12/org/scalatest/matchers/index.html
class IndicatorsTest extends AnyPropSpec with TableDrivenPropertyChecks with should.Matchers {

  val eps: Double = 0.05
  private val between = (exp: Double, act: Double) => act > exp - eps && act < exp + eps


  property("RSI on initialize") {
    forEvery(TestCases.RSIInitCases) { set =>
      val rsi = new RSI(15)
      val rsiResult = rsi.initialize(set._1)
      println("Rsi is equal to: " + rsiResult)
      between(set._2, rsiResult) shouldBe true
    }
  }

  property("RSI update") {
    forEvery(TestCases.RSIUpdateCases) { input =>
      val rsi = new RSI(15)
      rsi.initialize(input._1)
      val rsiResult = rsi.update(input._2)
      println("Rsi is equal to: " + rsiResult)
      between(input._3, rsiResult) shouldBe true
    }
  }

  property("OBV initialize") {
    forEvery(TestCases.OBVInitCases) { input =>
      val obv = new OBV
      val obvResult = obv.initialize(input._1)
      println("Obv is equal to: " + obvResult)
      obvResult shouldBe input._2
    }
  }

  property("OBV update") {
    forEvery(TestCases.OBVUpdateCases) { input =>
      val obv = new OBV
      obv.initialize(input._1)
      val obvResult = obv.update((input._2, input._3))
      println("Obv is equal to: " + obvResult)
      obvResult shouldBe input._4
    }
  }

  property("Support not be detected") {
    forEvery(TestCases.SupportAbsentInitCases) { input =>
      val supp = new Support(7)
      val actual = supp.initialize(input._1)
      println("Support not detected: " + actual._1 + " weight: " + actual._2)
      actual._2 should be < input._2
    }
  }

  property("Support be detected") {
    forEvery(TestCases.SupportPresentInitCases) { input =>
      val supp = new Support(7)
      val actual = supp.initialize(input._1)
      println("Support detected: " + actual._1 + " weight: " + actual._2)
      actual._1 shouldBe input._3
      actual._2 should be >= input._2.asInstanceOf[Double]
    }
  }

  property("Resistance not be detected") {
    forEvery(TestCases.ResistanceAbsentInitCases) { input =>
      val supp = new Resistance(7)
      val actual = supp.initialize(input._1)
      println("Resistance not detected: " + actual._1 + " weight: " + actual._2)
      actual._2 < input._2 shouldBe true
    }
  }

  property("Resistance be detected") {
    forEvery(TestCases.ResistancePresentInitCases) { input =>
      val supp = new Resistance(7)
      val actual = supp.initialize(input._1)
      println("Resistance detected: " + actual._1 + " weight: " + actual._2)
      println("Resistance expected: " + input._3 + " weight: " + input._2)
      actual._1 shouldBe input._3.asInstanceOf[Double]
      actual._2 >= input._2.asInstanceOf[Double] shouldBe true
    }
  }

  property("Peaks detected") {
    forEvery(TestCases.PeaksCases) { input =>
      val t = new TechnicalCalculator()
      val actual = t.findPeaks(input._1, input._2, input._3)
      actual._1 shouldBe input._4._1
      actual._2 shouldBe input._4._2
    }
  }

  property("Divergences detected") {
    forEvery(TestCases.DivergencesCases) { input =>
      val t = new TechnicalCalculator()
      val actual = t.findDivergences(input._1, input._2)
      actual should equal (input._3 +- 0.25)
    }
  }
}



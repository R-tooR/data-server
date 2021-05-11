import calculator.TechnicalCalculator
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec

class TechnicalCalculatorTest  extends AnyPropSpec with TableDrivenPropertyChecks with should.Matchers {
// AnyFlatSpec
//  "Technical calculator initialization " must "be with 20 records" in {
//    val tc = new TechnicalCalculator()
//    tc.initialize()
//  }

  property("Technical calculator on initialize") {
    forEvery(TestCases.TechnicalCalculatorInitializeCases) { set =>
      val tc = new TechnicalCalculator
      val actual = tc.initialize(set._1)
      tc.RSIBuffer.length shouldBe set._2
      tc.OBVBuffer.length shouldBe set._2
      math.abs(actual) should be <= set._3
    }
  }

  property("RSI on initialize") {
    forEvery(TestCases.TechnicalCalculatorUpdateCases) { set =>
      val tc = new TechnicalCalculator
      tc.initialize(set._1)
      val actual = for (datum <- set._2) yield tc.update(datum)
//      tc.RSIBuffer.length shouldBe set._2
//      tc.OBVBuffer.length shouldBe set._2
//      math.abs(actual) should be <= set._3
    }
  }
}

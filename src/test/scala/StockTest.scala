import org.scalatest.matchers.should
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.propspec.AnyPropSpec

class StockTest extends AnyPropSpec with should.Matchers {

  property("Full simulation of Stock")   {
    val stock = new Stock("angi.csv")
    stock.initialize()
    while(!stock.end){
      stock.update()
    }
    println(stock.calculator.RSIpeaks)
    println(stock.calculator.OBVpeaks)
    println(stock.calculator.ClosePricePeaks)
  }
}

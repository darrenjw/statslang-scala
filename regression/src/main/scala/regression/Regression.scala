
package regression

object Regression {

  def main(args: Array[String]): Unit = {

    // **********************************
    // Interactive session starts here

    import regression._
    import scala.math.log
    import org.saddle.io._
    import FrameUtils._

    val file = CsvFile("data/regression.csv")
    val df = CsvParser.parse(file).withColIndex(0)
    println(df)
    framePlot(getCol("Age", df), getCol("OI", df))

    val df2 = frameFilter(df, getCol("Age", df), _ > 0.0)
    println(df2)
    val oi = getCol("OI", df2)
    val age = getCol("Age", df2)
    val sex = getFactor("Sex", df2)
    framePlot(age, oi, sex).saveas("data.png")

    val y = oi.mapValues { log(_) }
    val m = Lm(y, List(age, sex))
    println(m)
    m.plotResiduals.saveas("resid.png")

    val summ = m.summary
    println(summ)

    // Interactive session ends here
    // ***********************************

  }

}

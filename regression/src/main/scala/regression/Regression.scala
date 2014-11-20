
package regression

object Regression {

  import scala.math.log
  import breeze.linalg.{ DenseMatrix, DenseVector }
  import breeze.plot._
  import org.saddle.io._
  import FrameUtils._

  def main(args: Array[String]): Unit = {

    // **********************************
    // Interactive session starts here

    val file = CsvFile("data/regression.csv")
    val df = CsvParser.parse(file).withColIndex(0)
    println(df)
    framePlot(getCol("Age",df),getCol("OI",df))

    val df2 = frameFilter(df, getCol("Age", df), _ > 0.0)
    println(df2)
    val oi = getCol("OI", df2)
    val age = getCol("Age", df2)
    val sex = getColF("Sex", df2)
    framePlot(age,oi)
    
    val oiM = frameFilter(oi, sex, _ == 1.0)
    val oiF = frameFilter(oi, sex, _ == 0.0)
    val ageM = frameFilter(age, sex, _ == 1.0)
    val ageF = frameFilter(age, sex, _ == 0.0)

    val f0 = Figure()
    val p0 = f0.subplot(0)
    p0 += plot(frame2vec(ageM), frame2vec(oiM), '.')
    p0 += plot(frame2vec(ageF), frame2vec(oiF), '.', "red")
    p0.xlabel = "Age"
    p0.ylabel = "OI"
    p0.title = "OI against age"
    f0.saveas("data.png")

    val y = oi.mapValues { log(_) }
    val m = Lm(y, List(age, sex))
    println(m)
    m.plotResiduals

    val sum = m.summary
    println(sum)

    // Interactive session ends here
    // ***********************************

  }

}

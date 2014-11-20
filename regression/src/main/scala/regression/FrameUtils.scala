package regression

import breeze.linalg.{ DenseMatrix, DenseVector }
import breeze.plot._
import org.saddle._
import org.saddle.io._

object FrameUtils {

  def frameFilter[S, U, T](f1: Frame[S, U, T], f2: Frame[S, U, Double], p: Double => Boolean): Frame[S, U, T] = {
    f1.row(f2.rfilter(x => p(x.at(0).get)).rowIx.toVec)
  }

  def frame2mat[T, S](df: Frame[T, S, Double]): DenseMatrix[Double] = {
    // DenseMatrix(df.numRows,df.numCols,df.toMat.contents)
    // TODO: The above doesn't seem to work for some reason, so loop over columns instead as a temp hack
    val X = DenseMatrix.zeros[Double](df.numRows, df.numCols)
    val M = df.toMat
    for (i <- 0 until df.numCols) {
      X(::, i) := DenseVector(M.takeCols(i).contents)
    }
    X
  }

  def frame2vec[T, S](df: Frame[T, S, Double]): DenseVector[Double] = DenseVector(df.toMat.takeCols(0).contents)

  def mat2frame(M: DenseMatrix[Double], rowIx: Index[String], colIx: Index[String]): Frame[String, String, Double] = {
    val SM = Mat(M.rows, M.cols, M.t.toArray)
    Frame(SM, rowIx, colIx)
  }

  def joinFrames[T, S, U](frames: List[Frame[T, S, U]]): Frame[T, S, U] = {
    // TODO: Use pattern matching!
    if (frames.length == 1) frames.head else
      frames.head.joinPreserveColIx(joinFrames(frames.tail))
  }

  def getCol[T](colName: String, sdf: Frame[T, String, String]): Frame[T, String, Double] = {
    sdf.col(colName).mapValues(CsvParser.parseDouble)
  }

  def getColS[T, U](colName: String, sdf: Frame[T, String, U]): Frame[T, String, U] = {
    sdf.col(colName)
  }

  // TODO: Uses treatment contrasts - should allow sum contrasts, too, at least
  def getFactor(colName: String, sdf: Frame[Int, String, String]): Frame[Int, String, Double] = {
    val sCol = getColS(colName, sdf)
    val levels = sCol.colAt(0).toVec.contents.toList.groupBy(identity).mapValues(_.length).keys.toList.sorted
    val dummies = levels.tail
    val dummyLabels = dummies.map { colName + _ }
    val boolFrame = joinFrames(dummies.map { dl => sCol.mapValues(_ == dl) })
    val modFrame = boolFrame.mapValues(x => if (x == true) 1.0 else 0.0)
    Frame(modFrame.toMat, modFrame.rowIx, Index(dummyLabels.toArray))
  }

  def framePlot[T](x: Frame[T, String, Double], y: Frame[T, String, Double]): Figure = {
    val f = Figure()
    val p = f.subplot(0)
    p += plot(frame2vec(x), frame2vec(y), '.')
    p.xlabel = x.colIx.uniques.toSeq.head
    p.ylabel = y.colIx.uniques.toSeq.head
    p.title = p.ylabel + " against " + p.xlabel
    f
  }

  def framePlot[T](x: Frame[T, String, Double], y: Frame[T, String, Double], f: Frame[T, String, Double]): Figure = {
    val colours = Vector("red", "green", "black", "cyan", "magenta", "yellow", "blue")
    val fig = Figure()
    val p = fig.subplot(0)
    var ind = f.rfilter(_.sum < 0.5).rowIx.toVec
    var xi = x.row(ind)
    var yi = y.row(ind)
    p += plot(frame2vec(xi), frame2vec(yi), '.')
    for (i <- 0 until f.numCols) {
      ind = f.rfilter(_.at(i).get == 1.0).rowIx.toVec
      xi = x.row(ind)
      yi = y.row(ind)
      p += plot(frame2vec(xi), frame2vec(yi), '.', colours(i))
    }
    p.ylabel = y.colIx.uniques.toSeq.head
    p.xlabel = x.colIx.uniques.toSeq.head
    p.title = p.ylabel + " against " + p.xlabel
    fig
  }

}


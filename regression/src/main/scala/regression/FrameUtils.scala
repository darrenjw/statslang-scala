package regression

import breeze.linalg.{ DenseMatrix, DenseVector }
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

  def getColF[T, U](colName: String, sdf: Frame[T, String, U]): Frame[T, String, Double] = {
    getColS(colName, sdf).mapValues(x => if (x == "Male") 1.0 else 0.0)
  }

}
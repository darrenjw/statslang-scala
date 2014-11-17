/*
LinAlgTest.scala

Some basic tests for linear algebra

*/

package regression

import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.junit.runner.RunWith

import breeze.stats.distributions._
import breeze.linalg._
import breeze.numerics._
import org.saddle._

@RunWith(classOf[JUnitRunner])
class LinAlgTest extends FunSuite {

  // Code for testing approximate equality of Doubles
  val eps = 0.00001
  def approxeq(x: Double, y: Double): Boolean = abs(x - y) < eps

  // set up a simple linear model (with perfect fit)
  val x = linspace(0, 10, 11)
  val y = (x * 0.5) + 2.0
  val xf = Frame[Int, String, Double](Vec(x.toArray), Index((0 to 10).toArray), Index("X"))
  val yf = Frame[Int, String, Double](Vec(y.toArray), Index((0 to 10).toArray), Index("Y"))
  val m = Lm(yf, List(xf))

  // For some reason I always include this test...   ;-)
  test("1+2=3") {
    assert(1 + 2 === 3)
  }

  test("Lm perfect fit") {
    assert(m.names.length === 2)
    assert(approxeq(m.coefficients(0, 0), 2.0))
    assert(approxeq(m.coefficients(1, 0), 0.5))
    assert(approxeq(sum(abs(m.residuals)), 0.0))
  }

  test("Backsolve vector") {
    val u = DenseMatrix((1.0, 2.0), (0.0, 3.0))
    val v = DenseVector(14.0, 15.0)
    val x = m.backSolve(u, v)
    assert(approxeq(x(0), 4.0))
    assert(approxeq(x(1), 5.0))
  }

  test("Backsolve matrix") {
    val u = DenseMatrix((1.0, 2.0), (0.0, 3.0))
    val v = DenseMatrix((14.0, 5.0), (15.0, 6.0))
    val x = m.backSolve(u, v)
    assert(approxeq(x(0, 0), 4.0))
    assert(approxeq(x(0, 1), 1.0))
    assert(approxeq(x(1, 0), 5.0))
    assert(approxeq(x(1, 1), 2.0))
  }

  test("thinQR") {
    val a = DenseMatrix((1.0, 2.0), (3.0, 2.0), (3.0, 5.0))
    val qr = m.thinQR(a)
    val q = qr._1
    val r = qr._2
    assert(q.rows === 3)
    assert(q.cols === 2)
    assert(r.rows === 2)
    assert(r.cols === 2)
    assert(approxeq(r(1, 0), 0.0))
    val id = (q.t) * q
    assert(id.rows === 2)
    assert(id.cols === 2)
    assert(approxeq(id(0, 0), 1.0))
    assert(approxeq(id(0, 1), 0.0))
    assert(approxeq(id(1, 0), 0.0))
    assert(approxeq(id(1, 1), 1.0))
    val aa = q * r
    assert(aa.rows === 3)
    assert(aa.cols === 2)
    val z = a - aa
    assert(approxeq(sum(abs(z)), 0.0))
  }

}





/* eof */



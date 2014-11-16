/*
monte-carlo.scala

*/

import scala.math._
import breeze.stats.distributions.Uniform
import breeze.linalg._
import scala.annotation.tailrec

object MonteCarlo {

  def main(args: Array[String]) = {
    val N = 1000000 // 10^7 is as big as mc1() can really cope with
    println("Running with "+N+" iterations")
    println("Idiomatic vectorised solution")
    time { println(5.0*mc1(N)/N) }
    println("Fast efficient (serial) tail call")
    time { println(5.0*mc2(N)/N) }
    println("Parallelised version")
    time { println(5.0*mc3(N)/N) }
    println("Done")
  }

  def f(x: Double): Double = math.exp(-x*x/2)/math.sqrt(2*Pi)

  // idiomatic Breeze solution
  def mc1(its: Int): Int = {
    val x = runif(its,-5.0,5.0)
    val y = runif(its,0.0,0.5)
    val fx = x map {f(_)}
    sum((y :< fx) map {xi => if (xi == true) 1 else 0})
  }

  // fast, memory-efficient tail call
  def mc2(its: Long): Long = {
    @tailrec def mc(its: Long,acc: Long): Long = {
      if (its == 0) acc else {
        val x = runif(-5.0,5.0)
        val y = runif(0.0,0.5)
        if (y < f(x)) mc(its-1,acc+1) else mc(its-1,acc)
      }  
    }
    mc(its,0)
  }

  // parallel version
  def mc3(its: Long): Long = {
    val NP = 8 // Max number of threads to use - can be more than number of cores
    val N = its/NP // assuming NP|its
    (1 to NP).toList.par.map{x => mc2(N)}.sum
  }

  // R-like functions for Uniform random numbers
  def runif(n: Int, l: Double, u: Double) = DenseVector[Double](Uniform(l,u).sample(n).toArray)
  def runif(l: Double,u: Double) = Uniform(l,u).sample

  // function for timing
  def time[A](f: => A) = {
    val s = System.nanoTime
    val ret = f
    println("time: "+(System.nanoTime-s)/1e6+"ms")
    ret 
  }

}





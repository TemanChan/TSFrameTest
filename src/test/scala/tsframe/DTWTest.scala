package tsframe

import tsframe.DTW._
import org.scalatest.FunSuite

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class DTWSuite extends FunSuite {
    test("A simple test"){
        // test case
        // note that A and B must have the same length because we use the cummulative bound obtained from LBKeogh
        val A = Array(1, 1, 2, 3, 2, 0)
        val B = Array(0, 1, 1, 2, 3, 2)
        val cb = Array.ofDim[Double](A.length)
        val window_size = A.length
        val bsf = scala.Double.MaxValue
        val dist = (a: Int, b: Int) => ((a - b) * (a - b)).toDouble
        val d1 = DTWCalculator(dist)(A, B, cb, window_size, bsf) 
        val d2 = SimpleDTW(dist)(A, B)
        assert(d1 == d2)
    }

    test("SimpleDTW") {
        val bsf = scala.Double.MaxValue
        val dist = (a: Double, b: Double) => (a - b) * (a - b)
        val arrayTupleGen = for {
            size <- Gen.choose(1, 200)
            A <- Gen.containerOfN[Array, Double](size, Gen.choose(-100.0, 100.0))
            B <- Gen.containerOfN[Array, Double](size, Gen.choose(-100.0, 100.0))
        } yield (A, B)
        val p = forAll(arrayTupleGen){ tuple =>
            val A = tuple._1
            val B = tuple._2
            val cb = Array.ofDim[Double](A.length)
            val r = A.length
            val d1 = DTWCalculator(dist)(A, B, cb, r, bsf)
            val d2 = SimpleDTW(dist)(A, B)
            println("" + d1 +", " + d2)
            d1 == d2
        }
        p.check
    }

}
